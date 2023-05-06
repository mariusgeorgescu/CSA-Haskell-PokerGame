{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

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

import           Cards                     (shuffleDeck, shuffleDeckR)
import           Control.Exception         (SomeException, evaluate, try)
import           Data.Bifunctor            (Bifunctor (first))
import           PokerGame                 (GameState (..),
                                            PokerGame (gameDeck),
                                            PokerPlayerAction (..), actionVal,
                                            addPlayerToGame, determineWinner,
                                            discardAndDraw, gameMaxBet,
                                            gameState, getCurrentPlayer,
                                            getCurrentPlayerBets,
                                            getCurrentPlayerId,
                                            getCurrentRoundPot, isFoldPlayer,
                                            mkFiveCardDrawPokerGame,
                                            mkGameSettings, roundBettingAction,
                                            showPlayerInGame)
import           PokerLogic                (Combination (..))
import           System.Console.Haskeline  (defaultSettings, getInputChar,
                                            runInputT)
import           Text.Read                 (readEither)

data Env =
  Env
    { envMinBet       :: Int
    , envMinPlayers   :: Int
    , envPermutations :: Maybe [Int]
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

-- | Function
pokerGameApp :: PokerApp ()
pokerGameApp = do
  initGame
  shuffleGameDeck =<< asks envPermutations
  addPlayers
  firstBettingRound
  drawingRound
  secondBettingRound
  printWinner
  return ()

initGame :: PokerApp ()
initGame = do
  minBet <- asks envMinBet
  playersToStart <- asks envMinPlayers
  liftIO $ gameMessage "STARTING GAME"
  gs <- liftEither $ mkGameSettings minBet playersToStart
  let g = mkFiveCardDrawPokerGame gs
  liftIO $ print g
  put g

shuffleGameDeck :: Maybe [Int] -> PokerApp ()
shuffleGameDeck permutations = do
  g <- get
  case permutations of
    Just p -> do
      sd <- liftEither $ shuffleDeck p $ gameDeck g
      put g {gameDeck = sd}
    Nothing -> do
      sd <- liftIO $ shuffleDeckR $ gameDeck g
      put g {gameDeck = sd}

addPlayers :: PokerApp ()
addPlayers = do
  liftIO $ gameMessage "WAITING FOR PLAYERS TO JOIN THE GAME"
  _ <- iterateUntil (== FirstBetRound) addPlayerAndReturnState
  g <- get
  liftIO $ gameMessage "SETTING DEALER | POSTING BLIND | DEALING HANDS"
  liftIO $ print g
  where
    addPlayerAndReturnState :: PokerApp GameState
    addPlayerAndReturnState = gameState <$> addPlayer
    addPlayer :: PokerApp PokerGame
    addPlayer = do
      name <- liftIO $ putStrLn "Enter your name: " >> getLine
      liftIO $ putStrLn "100 chips by default"
      g <- get
      g1 <-
        catchError
          (liftEither $ addPlayerToGame name 100 2 g)
          (handleLocalError addPlayer)
      put g1
      return g1

firstBettingRound :: PokerApp ()
firstBettingRound = do
  liftIO $ gameMessage "1st BETTING ROUND"
  s <-
    iterateUntil
      (`elem` [Drawing, Showdown, EndOfHand])
      bettingActionAndReturnState
  liftIO $ print s
  game <- get
  liftIO $ print game

secondBettingRound :: PokerApp ()
secondBettingRound = do
  liftIO $ gameMessage "2nd BETTING ROUND"
  s <- iterateUntil (`elem` [Showdown, EndOfHand]) bettingActionAndReturnState
  liftIO $ print s
  game <- get
  liftIO $ print game

bettingActionAndReturnState :: PokerApp GameState
bettingActionAndReturnState = gameState <$> bettingAction
  where
    bettingAction :: PokerApp PokerGame
    bettingAction = do
      liftIO $ gameMessage "BETTING ACTION"
      liftIO $ putStrLn "Player's Turn : "
      printCurrentPlayer
      game <- get
      liftIO $
        putStrLn $
        "Current max bet is: " ++ maybe "not set" show (gameMaxBet game)
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
            "| \ESC[92mC - Call\\Check \ESC[0m | \ESC[95mR - Raise\ESC[0m | \ESC[91mF - Fold\ESC[0m |" --- | \ESC[92mA - All In\ESC[0m |"
        charToAction :: Char -> PokerApp (Either String PokerPlayerAction)
        charToAction c
          | toUpper c == 'C' = do
            g <- get
            valToCall <- liftEither $ maybeToEither "" $ gameMaxBet g
            let playerBets = sum $ actionVal <$> getCurrentPlayerBets g
            return $
              Right $
              Call
                (if valToCall > playerBets
                   then valToCall - playerBets
                   else 0)
          | toUpper c == 'R' = do
            i <- liftEither =<< liftIO (getIntFromTerminal "Enter amount")
            g2 <- get
            liftIO $ print g2
            return $ Right $ Raise i
          | toUpper c == 'F' = return $ Right FoldHand
          | toUpper c == 'A' = return $ Left "All in not implemented"
          | otherwise        = return $ Left "Invalid option"

printWinner :: PokerApp ()
printWinner = do
  g <- get
  let results = determineWinner g
  let (wid, wcomb) = head results
  liftIO $ gameMessage "SHOWDOWN"
  _ <- liftIO $ mapM print results
  liftIO $
    gameMessage
      ("The winner is player : " ++
       show wid ++ " with " ++ show (combHandRank <$> wcomb))
  liftIO $ putStrLn $ showPlayerInGame g wid
  let pot = getCurrentRoundPot g
  liftIO $ gameMessage $ "This round pot is " ++ show pot
  return ()

drawingRound :: PokerApp ()
drawingRound = do
  liftIO $ gameMessage "DRAWING ROUND"
  s <- iterateUntil (== SecondBetRound) drawingActionAndReturnState
  liftIO $ print s
  game <- get
  liftIO $ print game
  where
    drawingActionAndReturnState :: PokerApp GameState
    drawingActionAndReturnState = gameState <$> drawingAction
      where
        drawingAction :: PokerApp PokerGame
        drawingAction = do
          liftIO $ gameMessage "DRAWING ACTION"
          game <- get
          printCurrentPlayer
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
              liftIO $ print g
              return g
          where
            getCardsToDiscard :: PokerApp [Int]
            getCardsToDiscard = do
              liftIO $ putStrLn "Do you want to discard cards ?"
              liftIO $
                putStrLn "| \ESC[92mY - Yes\ESC[0m | \ESC[95mN - NO\ESC[0m |"
              c <- liftIO getCharFromTerminal
              case toUpper <$> c of
                Just 'Y' -> getIndicesOfCardsToDiscard
                Just 'N' -> return []
                _ ->
                  throwError "Invalid action... choose Y or N" `catchError`
                  handleLocalError getCardsToDiscard
              where
                getIndicesOfCardsToDiscard :: PokerApp [Int]
                getIndicesOfCardsToDiscard = do
                  liftIO $
                    putStrLn
                      "Enter maximum 3 indices of cards to discard  \n \t ...(from 0 to 4:  "
                  line <- liftIO getLine
                  ints <- liftIO $ mapM safeDigitToInt line
                  catchError
                    (liftEither $ first show (sequence ints))
                    (handleLocalError getIndicesOfCardsToDiscard)
                  where
                    safeDigitToInt :: Char -> IO (Either SomeException Int)
                    safeDigitToInt c = try (evaluate (digitToInt c))

--
-- Others
--
printCurrentPlayer :: PokerApp ()
printCurrentPlayer = do
  game <- get
  p_id <- liftEither $ getCurrentPlayerId game
  liftIO $ putStrLn $ showPlayerInGame game p_id

gameMessage :: String -> IO ()
gameMessage message =
  let msglen = length message
      pas = (170 - msglen) `div` 2
   in putStrLn $
      "\ESC[92m" ++
      replicate pas '*' ++
      "|  " ++ message ++ "  |" ++ replicate pas '*' ++ "\ESC[0m"

handleLocalError :: PokerApp a -> String -> PokerApp a
handleLocalError g e = do
  liftIO $ gameErrorMessage e
  liftIO $ putStrLn "Try again ... "
  g

gameErrorMessage :: String -> IO ()
gameErrorMessage message = putStrLn $ "\ESC[31m" ++ message ++ "\ESC[0m"

getCharFromTerminal :: IO (Maybe Char)
getCharFromTerminal =
  runInputT defaultSettings (getInputChar "Type your option and press enter: ")

getIntFromTerminal :: String -> IO (Either String Int)
getIntFromTerminal msg = do
  putStrLn msg
  readEither <$> getLine
--------------------------------------
