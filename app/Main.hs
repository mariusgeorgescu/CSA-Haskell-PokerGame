{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crypto.Hash

import Data.ByteString (ByteString)

exampleHashWith :: ByteString -> IO ()
exampleHashWith msg = do
    putStrLn $ "  sha1(" ++ show msg ++ ") = " ++ show (hashWith SHA1   msg)
    putStrLn $ "sha256(" ++ show msg ++ ") = " ++ show (hashWith SHA256 msg)



-- testRun2 :: [Int] -> Either String PokerGame
-- testRun2 perms =
--   initPokerGame perms 10 >>= addPlayerToGame "Marius" 100 >>=
--   addPlayerToGame "Andrei" 100 >>=
--   startPokerGame >>=
--   setDealer 1 >>=
--   dealHands
-- randomList :: RandomGen g => g -> Int -> [Int]
-- randomList gen n = go gen [0 .. n - 1]
--   where
--     go _ [] = []
--     go g (x:xs) =
--       let (r, g') = randomR (0, n - x - 1) g
--        in r : go g' xs

main :: IO ()
main = do
  exampleHashWith "marius"