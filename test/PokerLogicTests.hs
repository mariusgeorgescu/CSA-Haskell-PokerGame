{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module PokerLogicTests where

import           Cards      (Card)
import           PokerGame  (mkHand)
import           PokerLogic (Combination (handRank, structure),
                             HandRank (Flush, FourOfaKind, FullHouse, Highcard, OnePair, RoyalFlush, Straight, StraightFlush, ThreeOfaKind, TwoPairs),
                             evaluateHand)
import           Test.Hspec (describe, hspec, it, shouldBe)

pokerHandEvaluateTestSuite :: IO ()
pokerHandEvaluateTestSuite =
  hspec $ do
    describe "EXAMPLE BASED TESTS - HAND EVALUATION" $ do
      describe "Test that the function correctly identifies a royal flush:" $ do
        it "Input: Ah, Kh, Qh, Jh, 10h, Expected output: Royal Flush" $ do
          let h = mkHand (["Ah", "Kh", "Qh", "Jh", "10h"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right RoyalFlush
      describe "Test that the function correctly identifies a straight flush:" $ do
        it "Input: 2d, 3d, 4d, 5d, 6d Expected output: Straight Flush" $ do
          let h = mkHand (["2d", "3d", "4d", "5d", "6d"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right StraightFlush
      describe "Test that the function correctly identifies four of a kind:" $ do
        it "Input: As, Ac, Ad, Ah, 2h Expected output: Four of a Kind" $ do
          let h = mkHand (["As", "Ac", "Ad", "Ah", "2h"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right FourOfaKind
      describe "Test that the function correctly identifies a full house:" $ do
        it "Input: Ks, Kc, Kd, Qh, Qs Expected output: Full House" $ do
          let h = mkHand (["Ks", "Kc", "Kd", "Qh", "Qs"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right FullHouse
      describe "Test that the function correctly identifies a flush:" $ do
        it "Input: 2s, 7s, 9s, Js, Qs Expected output: Flush" $ do
          let h = mkHand (["2s", "7s", "9s", "Js", "Qs"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right Flush
      describe "Test that the function correctly identifies a straight:" $ do
        it "Input: 2s, 3c, 4d, 5h, 6s Expected output: Straight" $ do
          let h = mkHand (["2s", "3c", "4d", "5h", "6s"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right Straight
      describe "Test that the function correctly identifies three of a kind:" $ do
        it "Input: 7h, 7c, 7d, 3s, 10c Expected output: Three of a Kind" $ do
          let h = mkHand (["7h", "7c", "7d", "3s", "10c"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right ThreeOfaKind
      describe "Test that the function correctly identifies two pair:" $ do
        it "Input: 9h, 9c, 10d, 10s, 4d Expected output: Two Pair" $ do
          let h = mkHand (["9h", "9c", "10d", "10s", "4d"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right TwoPairs
      describe "Test that the function correctly identifies one pair:" $ do
        it "Input: 6h, 6c, 3d, 5s, 10h Expected output: One Pair" $ do
          let h = mkHand (["6h", "6c", "3d", "5s", "10h"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right OnePair
      describe "Test that the function correctly identifies high card:" $ do
        it "Input: 4h, 7c, 8d, 10s, Qh Expected output: High Card" $ do
          let h = mkHand (["4h", "7c", "8d", "10s", "Qh"] :: [Card])
          (handRank . evaluateHand <$> h) `shouldBe` Right Highcard

pokerHandCompareTestSuite :: IO ()
pokerHandCompareTestSuite =
  hspec $ do
    describe "EXAMPLE BASED TESTS - HAND COMPARISION" $ do
      describe "Test that two royal flushes are equal:" $ do
        it
          "Input: [Ah, Kh, Qh, Jh, 10h] vs. [Ac, Kc, Qc, Jc, 10c] Expected output: Equal" $ do
          let h1 = mkHand (["Ah", "Kh", "Qh", "Jh", "10h"] :: [Card])
          let h2 = mkHand (["Ac", "Kc", "Qc", "Jc", "10c"] :: [Card])
          compare (evaluateHand <$> h1) (evaluateHand <$> h2) `shouldBe` EQ
      describe "Test that a royal flush beats a straight flush:" $ do
        it
          "Input: [Ah, Kh, Qh, Jh, 10h] vs. [2d, 3d, 4d, 5d, 6d] Expected output: First Hand Wins" $ do
          let h1 = mkHand (["Ah", "Kh", "Qh", "Jh", "10h"] :: [Card])
          let h2 = mkHand (["2d", "3d", "4d", "5d", "6d"] :: [Card])
          compare (evaluateHand <$> h1) (evaluateHand <$> h2) `shouldBe` GT
      describe
        "Test that two straight flushes with different high cards are compared correctly:" $ do
        it
          "Input: [2d, 3d, 4d, 5d, 6d] vs. [3c, 4c, 5c, 6c, 7c] Expected output: Second Hand Wins" $ do
          let h1 = mkHand (["2d", "3d", "4d", "5d", "6d"] :: [Card])
          let h2 = mkHand (["3c", "4c", "5c", "6c", "7c"] :: [Card])
          compare (evaluateHand <$> h1) (evaluateHand <$> h2) `shouldBe` LT
      describe "Test that a four of a kind beats a full house:" $ do
        it
          "Input: [As, Ac, Ad, Ah, 2h] vs. [Ks, Kc, Kd, Qh, Qs] Expected output: First Hand Wins" $ do
          let h1 = mkHand (["As", "Ac", "Ad", "Ah", "2h"] :: [Card])
          let h2 = mkHand (["Ks", "Kc", "Kd", "Qh", "Qs"] :: [Card])
          compare (evaluateHand <$> h1) (evaluateHand <$> h2) `shouldBe` GT
      describe
        "Test that two full houses with different three-of-a-kind values are compared correctly:" $ do
        it
          "Input: [Ks, Kc, Kd, 8h, 8s] vs. [Qs, Qc, Qd, Js, Jc] Expected output: First Hand Wins" $ do
          let h1 = mkHand (["Ks", "Kc", "Kd", "8h", "8s"] :: [Card])
          let h2 = mkHand (["Qs", "Qc", "Qd", "Js", "Jc"] :: [Card])
          compare (evaluateHand <$> h1) (evaluateHand <$> h2) `shouldBe` GT
      describe
        "Test that a flush beats a straight:" $ do
        it
          "Input: [2s, 7s, 9s, Js, Qs] vs. [2s, 3c, 4d, 5h, 6s] Expected output: First Hand Wins" $ do
          let h1 = mkHand (["2s", "7s", "9s", "Js", "Qs"] :: [Card])
          let h2 = mkHand (["2s", "3c", "4d", "5h", "6s"] :: [Card])
          compare (evaluateHand <$> h1) (evaluateHand <$> h2) `shouldBe` GT




-- Test that two flushes with different high cards are compared correctly:
-- Input: ["2s", "7s", "9s", "Js", "Qs"], ["3s", "8s", "10s", "Js", "Qs"]
-- Expected output: "Second Hand Wins"
-- Test that a three of a kind beats two pair:
-- Input: ["7h", "7c", "7d", "3s", "10c"], ["9h", "9c", "10d", "10s", "4d"]
-- Expected output: "First Hand Wins"
-- Test that two pairs with different high pairs are compared correctly:
-- Input: ["9h", "9c", "10d", "10s", "4d"], ["8h", "8c", "Jd", "Js", "2d"]
-- Expected output: "First Hand Wins"
-- Test that a pair beats a high card:
-- Input: ["6h", "6c", "3d", "5s", "10h"], ["4h", "7c", "8d", "10s", "Qh"]
-- Expected output: "First Hand Wins"

-- Test that two identical high card hands are equal:
-- Input: ["2h", "4d", "7c", "9s", "Jh"], ["2d", "4h", "7s", "9c", "Jd"]
-- Expected output: "Equal"
-- Test that two identical one pair hands are equal:
-- Input: ["3h", "3c", "5d", "9s", "Jh"], ["3d", "3s", "5h", "9c", "Jd"]
-- Expected output: "Equal"
-- Test that two identical two pair hands are equal:
-- Input: ["3h", "3c", "5d", "5s", "Jh"], ["3d", "3s", "5h", "5c", "Jd"]
-- Expected output: "Equal"
-- Test that two identical three of a kind hands are equal:
-- Input: ["3h", "3c", "3d", "5s", "Jh"], ["3s", "3d", "3c", "5h", "Jd"]
-- Expected output: "Equal"
-- Test that two identical straight hands are equal:
-- Input: ["2h", "3c", "4d", "5s", "6h"], ["2d", "3s", "4h", "5c", "6d"]
-- Expected output: "Equal"
-- Test that two identical flush hands are equal:
-- Input: ["2h", "4h", "6h", "8h", "10h"], ["2d", "4d", "6d", "8d", "10d"]
-- Expected output: "Equal"
-- Test that two identical full house hands are equal:
-- Input: ["3h", "3c", "3d", "Jh", "Js"], ["3s", "3d", "3c", "Jd", "Jc"]
-- Expected output: "Equal"
-- Test that two identical four of a kind hands are equal:
-- Input: ["3h", "3c", "3d", "3s", "Jh"], ["3d", "3c", "3s", "3h", "Jd"]
-- Expected output: "Equal"
-- Test that two identical straight flush hands are equal:
-- Input: ["2h", "3h", "4h", "5h", "6h"], ["2d", "3d", "4d", "5d", "6d"]
-- Expected output: "Equal"
-- Test that a hand with all identical cards is a valid hand:
-- Input: ["3h", "3h", "3h", "3h", "3h"], ["2d", "2d", "2d", "2d", "2d"]
-- Expected output: "Invalid Hand"
