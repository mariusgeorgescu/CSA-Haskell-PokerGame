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

import           Control.Monad             (replicateM_)
import           Control.Monad.Error.Class (MonadError (throwError))
import           Data.Either.Extra         (maybeToEither)
import           PokerGame                 (PokerGame (_gamePlayerTurnIndex),
                                            PokerPlayerAction (..),
                                            addPlayerToGame, dealHands,
                                            inGameBettingAction, initPokerGame,
                                            postBlinds, setDealer,
                                            startPokerGame)
import           System.Random             (Random (randomR), RandomGen,
                                            mkStdGen)

newtype Env =
  Env
    { envMinBet :: Int
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


-- | Function to initate a PokerGame in NotStarted state, with a valid Deck and mininum bet value.
testPokerApp :: PokerApp ()
testPokerApp = do
  initGame
  addPlayers 4
  g2 <- get
  g3 <- liftEither $ startPokerGame g2
  liftIO $ print g3
  g4 <- liftEither $ setDealer 0 g3
  liftIO $ print g4
  g5 <- liftEither $ postBlinds g4
  liftIO $ print g5
  g6 <- liftEither $ dealHands g5
  liftIO $ print g6
  pt <- liftEither $ maybeToEither "err" $ _gamePlayerTurnIndex g6
  g7 <- liftEither $ inGameBettingAction pt (Raise 10) g6
  liftIO $ print g7
  pt <- liftEither $ maybeToEither "err" $ _gamePlayerTurnIndex g7
  g8 <- liftEither $ inGameBettingAction pt FoldHand g7
  liftIO $ print g8
  pt <- liftEither $ maybeToEither "err" $ _gamePlayerTurnIndex g8
  g9 <- liftEither $ inGameBettingAction pt (Call 9) g8
  liftIO $ print g9
  return ()

initGame :: PokerApp ()
initGame = do
  minBet <- asks envMinBet
  let gen = mkStdGen 10
  let permutations = randomList gen 51
  liftIO $ print "INITIALISING POKER GAME"
  g <- liftEither $ initPokerGame permutations minBet
  put g

addPlayer :: PokerApp ()
addPlayer = do
  name <- liftIO $ print "Enter your name: " >> getLine
  liftIO $ print "100 chips by default"
  g <- get
  g1 <- liftEither $ addPlayerToGame name 100 g
  put g1

addPlayers :: Int -> PokerApp ()
addPlayers n = replicateM_ n addPlayer
