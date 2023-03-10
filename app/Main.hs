module Main where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import PokerGame (mkPokerPlayer, startPokerGame, setDealer, dealHands, PokerGame)

testRun :: Either String PokerGame
testRun = do
  player1 <- mkPokerPlayer 1 "marius" 100
  player2 <- mkPokerPlayer 2 "andrei" 100
  game <- startPokerGame 10 [player1, player2]
  game1 <- setDealer 1 game
  game2 <- dealHands game1
  return game2

main :: IO ()
main = do
  putStrLn "Hello, Haskell Poker Game!"
  print testRun
