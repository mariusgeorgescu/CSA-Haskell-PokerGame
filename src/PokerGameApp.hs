{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module PokerGameApp where

import           Control.Monad.Except      (ExceptT, MonadError (catchError),
                                            MonadTrans (lift), liftEither,
                                            runExceptT)
import           Control.Monad.Reader      (MonadIO (..),
                                            MonadReader (ask, local),
                                            ReaderT (..), asks)
import           Control.Monad.State       (MonadState, StateT (runStateT),
                                            evalStateT, get, put)

import           Control.Monad.Error.Class (MonadError (throwError))
import           Control.Monad.Loops       (iterateUntil)

import           Data.Either.Extra         (maybeToEither)
import           PokerGame                 (GameState (..),
                                            PokerGame (gamePlayerTurnIndex, gamePlayers),
                                            PokerPlayer (playerName),
                                            addPlayerToGame, gameState,
                                            getCurrentPlayer,
                                            getCurrentPlayerId,
                                            getCurrentPlayerName, initPokerGame,
                                            showPlayerInGame)
import           System.Random             (Random (randomR), RandomGen,
                                            mkStdGen)

data Env =
  Env
    { envMinBet     :: Int
    , envMinPlayers :: Int
    }

newtype PokerApp a =
  PokerApp
    { pokerApp :: ReaderT Env (StateT PokerGame (ExceptT String IO)) a
    }
  deriving (Functor, Applicative, Monad)

runPokerApp :: Env -> PokerGame -> PokerApp a -> IO a
runPokerApp env pg papp@(PokerApp app) =
  let readerAction = runReaderT app env
      stateAction = evalStateT readerAction pg
      exceptAction = runExceptT stateAction
   in do ioAction <- exceptAction
         case ioAction of
           Right a -> return a
           Left e -> do
             print e
             runPokerApp env pg papp -- re-run the app when error occurs

instance MonadIO PokerApp where
  liftIO :: IO a -> PokerApp a
  liftIO = PokerApp . liftIO

instance MonadReader Env PokerApp where
  ask :: PokerApp Env
  ask = PokerApp ask
  local :: (Env -> Env) -> PokerApp a -> PokerApp a
  local f (PokerApp m) = PokerApp $ local f m

instance MonadState PokerGame PokerApp where
  get :: PokerApp PokerGame
  get = PokerApp get
  put :: PokerGame -> PokerApp ()
  put s = PokerApp $ put s

instance MonadError String PokerApp where
  throwError :: String -> PokerApp a
  throwError = PokerApp . lift . lift . throwError
  catchError :: PokerApp a -> (String -> PokerApp a) -> PokerApp a
  catchError action handler =
    PokerApp $ do
      env <- ask
      game <- get
      result <-
        liftIO $ runExceptT $ runStateT (runReaderT (pokerApp action) env) game
      case result of
        Left err         -> pokerApp (handler err)
        Right (x, game') -> put game' >> return x

randomList :: RandomGen g => g -> Int -> [Int]
randomList gen n = go gen [0 .. n - 1]
  where
    go _ [] = []
    go g (x:xs) =
      let (r, g') = randomR (0, n - x - 1) g
       in r : go g' xs


-- | Function
testPokerApp :: PokerApp ()
testPokerApp = do
  initGame
  addPlayers
  bettingAction
  return ()

initGame :: PokerApp ()
initGame = do
  minBet <- asks envMinBet
  playersToStart <- asks envMinPlayers
  let gen = mkStdGen 10
  let permutations = randomList gen 51
  liftIO $ gameMessage "Initializing the game"
  g <- liftEither $ initPokerGame permutations minBet playersToStart
  liftIO $ print g
  put g

addPlayers :: PokerApp ()
addPlayers = do
  liftIO $ gameMessage "Waiting for players to join the game "
  _ <- iterateUntil (== FirstBetRound) addPlayer
  g <- get
  liftIO $ gameMessage "Setting dealer, posting small blinds and dealing hands"
  liftIO $ print g

addPlayer :: PokerApp GameState
addPlayer = do
  name <- liftIO $ print "Enter your name: " >> getLine
  liftIO $ print "100 chips by default"
  g <- get
  g1 <- liftEither $ addPlayerToGame name 100 2 g
  put g1
  return $ gameState g1

printCurrentPlayer :: PokerApp ()
printCurrentPlayer = do
  game <- get
  p_id <- liftEither $ getCurrentPlayerId game
  liftIO $ putStrLn $ showPlayerInGame game p_id

bettingAction :: PokerApp ()
bettingAction = do
  liftIO $ gameMessage "Betting Action"
  liftIO $ print "Player's Turn : "
  printCurrentPlayer
  return ()


--
-- Others
--
gameMessage :: String -> IO ()
gameMessage message =
  let msglen = length message
      pas = (170 - msglen) `div` 2
   in putStrLn $
      "\ESC[92m" ++
      replicate pas '*' ++
      "|  " ++ message ++ "  |" ++ replicate pas '*' ++ "\ESC[0m"
