{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CardsCrypt    (demo)
import           PokerGame     (initPokerGame)
import           PokerGameApp  (Env (Env), pokerGameApp, runPokerApp)
import           System.Random (mkStdGen)
import           Utilities     (randomList)

main :: IO ()
main = do
  putStrLn "Hello, Haskell Poker Game!"
  let gen = mkStdGen 124
  let perms = randomList gen 51
  let g = initPokerGame 10 4
  either print (flip (runPokerApp (Env 1 4 (Just perms))) pokerGameApp) g
  -- demo
