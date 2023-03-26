module Main where

import           PokerGame

import           PokerGameApp  (Env (Env), runPokerApp, testPokerApp)
import           System.Random (Random (randomR), RandomGen, newStdGen)


-- testRun2 :: [Int] -> Either String PokerGame
-- testRun2 perms =
--   initPokerGame perms 10 >>= addPlayerToGame "Marius" 100 >>=
--   addPlayerToGame "Andrei" 100 >>=
--   startPokerGame >>=
--   setDealer 1 >>=
--   dealHands
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
  let perms = randomList gen 51
  let g = initPokerGame perms 10 4
  either print (flip (runPokerApp (Env 1 4)) testPokerApp) g
