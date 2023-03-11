module Main where

import           PokerGame     (PokerGame, addPlayerToGame, dealHands,
                                initPokerGame, setDealer, startPokerGame)

import           System.Random (Random (randomR), RandomGen, newStdGen)

testRun2 :: [Int] -> Either String PokerGame
testRun2 perms =
  initPokerGame perms 10 >>= addPlayerToGame "Marius" 100 >>=
  addPlayerToGame "Andrei" 100 >>=
  startPokerGame >>=
  setDealer 1 >>=
  dealHands

initPlayer :: IO (String, Int)
initPlayer = do
  print "Enter your name: "
  name <- getLine
  print "Enter your chips: "
  chips <- getLine
  return (name, read chips)

randomList :: RandomGen g => g -> Int -> [Int]
randomList gen n = go gen [0 .. n - 1]
  where
    go _ [] = []
    go g (x:xs) =
      let (r, g') = randomR (0, n - x - 1) g
       in r : go g' xs

main :: IO ()
main = do
  putStrLn "Hello, Haskell Poker Game!"
  gen <- newStdGen
  let permutations = randomList gen 51
  print permutations
  print (length permutations)
  case testRun2 permutations of
    Left s -> print s
    Right pg -> do
      print pg
