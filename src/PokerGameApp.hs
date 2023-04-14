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

import           Data.Char                 (digitToInt, toUpper)
import           Data.Either.Extra         (maybeToEither)
import           PokerGame                 (GameState (..), PokerGame,
                                            PokerPlayerAction (..), actionVal,
                                            addPlayerToGame, discardAndDraw,
                                            gameMaxBet, gameState,
                                            getCurrentPlayer,
                                            getCurrentPlayerBets,
                                            getCurrentPlayerHand,
                                            getCurrentPlayerId, isFoldPlayer,
                                            mkFiveCardDrawPokerGame,
                                            mkGameSettings, roundBettingAction,
                                            showPlayerInGame, determineWinner)
import           System.Console.Haskeline  (defaultSettings, getInputChar,
                                            runInputT)
import           System.Random             (Random (randomR), RandomGen,
                                            mkStdGen)
import           Text.Read                 (readEither)

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
  firstBettingRound
  drawingRound
  secondBettingRound
  g <- get 
  let w = determineWinner g
  liftIO $ print w
  return ()

initGame :: PokerApp ()
initGame = do
  minBet <- asks envMinBet
  playersToStart <- asks envMinPlayers
  let gen = mkStdGen 10
  let permutations = randomList gen 51
  liftIO $ gameMessage "Initializing the game"
  gs <- liftEither $ mkGameSettings minBet playersToStart
  let g = mkFiveCardDrawPokerGame gs
  liftIO $ print g
  put g

addPlayers :: PokerApp ()
addPlayers = do
  liftIO $ gameMessage "Waiting for players to join the game "
  _ <- iterateUntil (== FirstBetRound) addPlayerAndReturnState
  g <- get
  liftIO $ gameMessage "Setting dealer, posting small blinds and dealing hands"
  liftIO $ print g
  where
    addPlayerAndReturnState :: PokerApp GameState
    addPlayerAndReturnState = gameState <$> addPlayer
    addPlayer :: PokerApp PokerGame
    addPlayer = do
      name <- liftIO $ print "Enter your name: " >> getLine
      liftIO $ print "100 chips by default"
      g <- get
      g1 <-
        catchError
          (liftEither $ addPlayerToGame name 100 2 g)
          (handleLocalError addPlayer)
      put g1
      return g1

firstBettingRound :: PokerApp ()
firstBettingRound = do
  liftIO $ gameMessage "First betting round"
  s <-
    iterateUntil
      (`elem` [Drawing, Showdown, EndOfHand])
      bettingActionAndReturnState
  liftIO $ print s
  game <- get
  liftIO $ print game

secondBettingRound :: PokerApp ()
secondBettingRound = do
  liftIO $ gameMessage "2nd betting round"
  s <- iterateUntil (`elem` [Showdown, EndOfHand]) bettingActionAndReturnState
  liftIO $ print s
  game <- get
  liftIO $ print game

bettingActionAndReturnState :: PokerApp GameState
bettingActionAndReturnState = gameState <$> bettingAction

bettingAction :: PokerApp PokerGame
bettingAction = do
  liftIO $ gameMessage "Betting Action"
  liftIO $ putStrLn "Player's Turn : "
  printCurrentPlayer
  game <- get
  liftIO $ print $ "Current max bet is: " ++ show (gameMaxBet game)
  currentPlayer <- liftEither $ getCurrentPlayer game
  user_action <-
    if isFoldPlayer currentPlayer
      then return Nothing
      else Just <$> getActionFromUser
  game2 <- get
  g <-
    catchError
      (liftEither (roundBettingAction user_action game2))
      (handleLocalError bettingAction)
  put g
  liftIO $ print g
  return g
  where
    getActionFromUser :: PokerApp PokerPlayerAction
    getActionFromUser = do
      liftIO $ putStrLn "Choose your action : "
      liftIO printAvailableActions
      c <- liftEither . maybeToEither "error" =<< liftIO getCharFromTerminal
      catchError
        (liftEither =<< charToAction c)
        (handleLocalError getActionFromUser)
    printAvailableActions :: IO ()
    printAvailableActions =
      putStrLn
        "| \ESC[92mC - Call\ESC[0m | \ESC[95mR - Raise\ESC[0m | \ESC[91mF - Fold\ESC[0m | \ESC[92mA - All In\ESC[0m |"
    charToAction :: Char -> PokerApp (Either String PokerPlayerAction)
    charToAction c
      | toUpper c == 'C' = do
        g <- get
        valToCall <- liftEither $ maybeToEither "" $ gameMaxBet g
        let playerBets = sum $ actionVal <$> getCurrentPlayerBets g
        return $ Right $ Call (valToCall - playerBets)
      | toUpper c == 'R' = do
        i <- liftEither =<< liftIO (getIntFromTerminal "Enter amount")
        g2 <- get
        liftIO $ print g2
        return $ Right $ Raise i
      | toUpper c == 'F' = return $ Right FoldHand
      | toUpper c == 'A' = return $ Left "All in not implemented"
      | otherwise        = return $ Left "Invalid option"

printCurrentPlayer :: PokerApp ()
printCurrentPlayer = do
  game <- get
  p_id <- liftEither $ getCurrentPlayerId game
  liftIO $ putStrLn $ showPlayerInGame game p_id


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

gameErrorMessage :: String -> IO ()
gameErrorMessage message = putStrLn $ "\ESC[31m" ++ message ++ "\ESC[0m"

handleLocalError :: PokerApp a -> String -> PokerApp a
handleLocalError g e = do
  liftIO $ gameErrorMessage e
  liftIO $ putStrLn "Try again ... "
  g

getCharFromTerminal :: IO (Maybe Char)
getCharFromTerminal =
  runInputT defaultSettings (getInputChar "Type your option and press enter: ")

getIntFromTerminal :: String -> IO (Either String Int)
getIntFromTerminal msg = do
  print msg
  readEither <$> getLine


--------------------------------------
drawingAction :: PokerApp PokerGame
drawingAction = do
  liftIO $ gameMessage "Drawing Action"
  game <- get
  p_id <- liftEither $ getCurrentPlayerId game
  liftIO $ putStrLn $ "Player's Turn : " ++ show p_id
  liftIO $ print $ getCurrentPlayerHand game
  player <- liftEither $ getCurrentPlayer game
  if isFoldPlayer player
    then do
      g <- liftEither $ discardAndDraw [] game
      put g
      return g
    else do
      to_discard <- getCardsToDiscard
      g <-
        catchError
          (liftEither $ discardAndDraw to_discard game)
          (handleLocalError drawingAction)
      put g
      return g

getCardsToDiscard :: PokerApp [Int]
getCardsToDiscard = do
  liftIO $ print "enter indexes of cards to discard: "
  line <- liftIO getLine
  let ints = digitToInt <$> line
  catchError (pure ints) (handleLocalError getCardsToDiscard)

drawingActionAndReturnState :: PokerApp GameState
drawingActionAndReturnState = gameState <$> drawingAction

drawingRound :: PokerApp ()
drawingRound = do
  liftIO $ gameMessage "Drawing round"
  s <- iterateUntil (== SecondBetRound) drawingActionAndReturnState
  liftIO $ print s
  game <- get
  liftIO $ print game
