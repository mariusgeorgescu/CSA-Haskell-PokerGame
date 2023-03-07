module UtilityFunctionsTests where

import           Cards           (Deck (getCards), intToRank, mkFullDeck,
                                  rankToInt, shuffleDeckR)
import           Data.List       (nub)

import           PokerGame

import           Data.Either     (isLeft)
import           Test.Hspec
import           Test.QuickCheck

propertyTestsSuite :: IO ()
propertyTestsSuite =
  hspec $ do
    describe "Utility funnctions" $ do
      it "intToRank should be inverse of rankToInt" $
        property $ \x ->
          x <= 13 && x >= 1 ==> rankToInt (intToRank x) `shouldBe` x
    describe "Deck" $ do
      let deck = mkFullDeck
      context "When created" $ do
        it "should contain 52 cards" $ do length (getCards deck) `shouldBe` 52
        it "should not contain duplicates" $ do
          nub (getCards deck) `shouldBe` getCards deck
      context "When shuffled" $ do
        it "should not contain duplicates" $ do
          shuffled <- shuffleDeckR deck
          nub (getCards shuffled) `shouldBe` getCards shuffled
        it "length must be preserved" $ do
          shuffled <- shuffleDeckR deck
          length (getCards shuffled) `shouldBe` 52
    describe "Hand" $ do
      context "When created" $ do
        it "should show an error if no. of cards is different than 5" $
          property $ \n ->
            n /=
            5 ==> do
              cards <- generate $ vectorOf n arbitrary
              isLeft (mkHand cards) `shouldBe` True
        it "should contain 5 cards" $
          property $ \h -> length (getHandCards h) `shouldBe` (5 :: Int)
        it "should not contain duplicates" $
          property $ \h -> nub (getHandCards h) `shouldBe` getHandCards h
