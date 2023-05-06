{-# LANGUAGE OverloadedStrings #-}

module PokerLogicTests where

import           Cards
import           PokerLogic (Combination (combHandRank), Hand (handCards),
                             HandRank (..), evaluateHand, mkHand)
import           Test.Hspec (describe, hspec, it, shouldBe)

pokerHandEvaluateTestSuite :: IO ()
pokerHandEvaluateTestSuite =
  hspec $ do
    describe "EXAMPLE BASED TESTS - HAND EVALUATION" $ do
      describe "Test that the function correctly identifies a royal flush:" $ do
        isHandRank ["Ah", "Kh", "Qh", "Jh", "10h"] RoyalFlush
      describe "Test that the function correctly identifies a straight flush:" $ do
        isHandRank ["2d", "3d", "4d", "5d", "6d"] StraightFlush
      describe "Test that the function correctly identifies four of a kind:" $ do
        isHandRank ["As", "Ac", "Ad", "Ah", "2h"] FourOfaKind
      describe "Test that the function correctly identifies a full house:" $ do
        isHandRank ["Ks", "Kc", "Kd", "Qh", "Qs"] FullHouse
      describe "Test that the function correctly identifies a flush:" $ do
        isHandRank ["2s", "7s", "9s", "Js", "Qs"] Flush
      describe "Test that the function correctly identifies a straight:" $ do
        isHandRank ["2s", "3c", "4d", "5h", "6s"] Straight
      describe "Test that the function correctly identifies three of a kind:" $ do
        isHandRank ["7h", "7c", "7d", "3s", "10c"] ThreeOfaKind
      describe "Test that the function correctly identifies two pair:" $ do
        isHandRank ["9h", "9c", "10d", "10s", "4d"] TwoPairs
      describe "Test that the function correctly identifies one pair:" $ do
        isHandRank ["6h", "6c", "3d", "5s", "10h"] OnePair
      describe "Test that the function correctly identifies high card:" $ do
        isHandRank ["4h", "7c", "8d", "10s", "Qh"] Highcard
  where
    isHandRank cards handRank = do
      it
        ("Input: " ++ show cards ++ "\t --> Expected output: " ++ show handRank) $ do
        let h = mkHand cards
        (combHandRank . evaluateHand . handCards <$> h) `shouldBe`
          Right handRank

pokerHandCompareTestSuite :: IO ()
pokerHandCompareTestSuite =
  hspec $ do
    describe "EXAMPLE BASED TESTS - HAND COMPARISION" $ do
      describe "Test that two royal flushes are equal:" $ do
        compareHands
          ["Ah", "Kh", "Qh", "Jh", "10h"]
          ["Ac", "Kc", "Qc", "Jc", "10c"]
          EQ
      describe "Test that a royal flush beats a straight flush:" $ do
        compareHands
          ["Ah", "Kh", "Qh", "Jh", "10h"]
          ["2d", "3d", "4d", "5d", "6d"]
          GT
      describe
        "Test that two straight flushes with different high cards are compared correctly:" $ do
        compareHands
          ["2d", "3d", "4d", "5d", "6d"]
          ["3c", "4c", "5c", "6c", "7c"]
          LT
      describe "Test that a four of a kind beats a full house:" $ do
        compareHands
          ["As", "Ac", "Ad", "Ah", "2h"]
          ["Ks", "Kc", "Kd", "Qh", "Qs"]
          GT
      describe
        "Test that two full houses with different three-of-a-kind values are compared correctly:" $ do
        compareHands
          ["Ks", "Kc", "Kd", "8h", "8s"]
          ["Qs", "Qc", "Qd", "Js", "Jc"]
          GT
      describe "Test that a flush beats a straight:" $ do
        compareHands
          ["2s", "7s", "9s", "Js", "Qs"]
          ["2s", "3c", "4d", "5h", "6s"]
          GT
      describe
        "Test that two flushes with different high cards are compared correctly:" $ do
        compareHands
          ["2s", "7s", "9s", "Js", "Qs"]
          ["3s", "8s", "10s", "Js", "Ks"]
          LT
      describe "Test that a three of a kind beats two pair:" $ do
        compareHands
          ["7h", "7c", "7d", "3s", "10c"]
          ["9h", "9c", "10d", "10s", "4d"]
          GT
      describe
        "Test that two pairs with different high pairs are compared correctly:" $ do
        compareHands
          ["9h", "9c", "10d", "10s", "4d"]
          ["8h", "8c", "Jd", "Js", "2d"]
          LT
      describe "Test that a pair beats a high card:" $ do
        compareHands
          ["6h", "6c", "3d", "5s", "10h"]
          ["4h", "7c", "8d", "10s", "Qh"]
          GT
      describe "Test that two identical high card hands are equal:" $ do
        compareHands
          ["2h", "4h", "6h", "8h", "10h"]
          ["2d", "4d", "6d", "8d", "10d"]
          EQ
      describe "Test that two identical one pair hands are equal:" $ do
        compareHands
          ["3h", "3c", "5d", "9s", "Jh"]
          ["3d", "3s", "5h", "9c", "Jd"]
          EQ
      describe "Test that one pair of Aces beats One pair of Kings:" $ do
        compareHands
          ["Ah", "Ac", "5d", "9s", "Jh"]
          ["Kd", "Ks", "5h", "9c", "Jd"]
          GT
      describe "Test that two identical two pair hands are equal:" $ do
        compareHands
          ["3h", "3c", "5d", "5s", "Jh"]
          ["3d", "3s", "5h", "5c", "Jd"]
          EQ
      describe "Test that two identical three of a kind hands are equal:" $ do
        compareHands
          ["3h", "3c", "3d", "5s", "Jh"]
          ["3s", "3d", "3c", "5h", "Jd"]
          EQ
      describe "Test that two identical straight hands are equal:" $ do
        compareHands
          ["2h", "3c", "4d", "5s", "6h"]
          ["2d", "3s", "4h", "5c", "6d"]
          EQ
      describe "Test that two identical flush hands are equal:" $ do
        compareHands
          ["2h", "4d", "7c", "9s", "Jh"]
          ["2d", "4h", "7s", "9c", "Jd"]
          EQ
      describe "Test that two identical full house hands are equal:" $ do
        compareHands
          ["3h", "3c", "3d", "Jh", "Js"]
          ["3d", "3c", "3s", "Jc", "Jd"]
          EQ
      describe "Test that two identical four of a kind hands are equal:" $ do
        compareHands
          (["3h", "3c", "3d", "3h", "Js"] :: [Card])
          (["3h", "3c", "3d", "3h", "Jd"] :: [Card])
          EQ
      describe "Test that two identical straight flush hands are equal:" $ do
        compareHands
          (["2h", "3h", "4h", "5h", "6h"] :: [Card])
          (["2d", "3d", "4d", "5d", "6d"] :: [Card])
          EQ
  where
    compareHands cards1 cards2 cmp = do
      it
        ("Input: " ++
         show cards1 ++
         " vs. " ++ show cards2 ++ "\t --> Expected output: " ++ show cmp) $ do
        compare (evaluateHand cards1) (evaluateHand cards2) `shouldBe` cmp
