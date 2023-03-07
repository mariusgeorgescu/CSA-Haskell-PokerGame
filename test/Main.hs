{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import           PokerLogicTests       (pokerHandCompareTestSuite,
                                        pokerHandEvaluateTestSuite)
import           UtilityFunctionsTests (propertyTestsSuite)

main :: IO ()
main = do
  putStrLn "Test suite"
  propertyTestsSuite
  pokerHandEvaluateTestSuite
  pokerHandCompareTestSuite
