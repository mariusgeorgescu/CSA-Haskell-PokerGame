{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CardsCrypt    (demo)
import           PokerGameApp  (pokerGameApp, runPokerApp)

import           PokerGame

import           PokerGameApp  (Env (Env), randomList)
import           System.Random

main :: IO ()
main = do
  putStrLn "Hello, Haskell Poker Game!"
  let gen = mkStdGen 124
  let perms = randomList gen 51
  let g = initPokerGame 10 4
  either print (flip (runPokerApp (Env 1 4 (Just perms))) pokerGameApp) g
  -- demo
