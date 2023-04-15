module UtilityFunctionsTests where

import           Cards           (Deck (deckCards), intToRank, mkFullDeck,
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
          x <= 12 && x >= 2 ==> rankToInt (intToRank x) `shouldBe` x
    describe "Deck" $ do
      let deck = mkFullDeck
      context "When created" $ do
        it "should contain 52 cards" $ do length (deckCards deck) `shouldBe` 52
        it "should not contain duplicates" $ do
          nub (deckCards deck) `shouldBe` deckCards deck
      context "When shuffled" $ do
        it "should not contain duplicates" $ do
          shuffled <- shuffleDeckR deck
          nub (deckCards shuffled) `shouldBe` deckCards shuffled
        it "length must be preserved" $ do
          shuffled <- shuffleDeckR deck
          length (deckCards shuffled) `shouldBe` 52
    describe "Hand" $ do
      context "When created" $ do
        it "should show an error if no. of cards is different than 5" $
          property $ \n ->
            n /=
            5 ==> do
              cards <- generate $ vectorOf n arbitrary
              isLeft (mkHand cards) `shouldBe` True
        it "should contain 5 cards" $
          property $ \h -> length (handCards h) `shouldBe` (5 :: Int)
        it "should not contain duplicates" $
          property $ \h -> nub (handCards h) `shouldBe` handCards h
