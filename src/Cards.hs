{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Cards
  ( Suit
  , Rank(..)
  , Card(getRank, getSuit)
  , Deck
  , mkFullDeck
  , rankToInt
  ) where

import           System.Random.Shuffle (shuffle)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, elements)

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

-- | Card suits.
data Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs
  deriving (Show, Eq, Enum, Bounded)


-- | Card ranks.
data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Card .
data Card =
  Card
    { getRank :: !Rank
    , getSuit :: !Suit
    }
  deriving (Eq)

instance Ord Card
 where
  (<=) :: Card -> Card -> Bool
  c1 <= c2 = getRank c1 <= getRank c2

instance Show Card where
  show :: Card -> String
  show (Card r s) = show r ++ " of " ++ show s

newtype Deck =
  Deck
    { getCards :: [Card]
    }
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Deck operation functions
-------------------------------------------------------------------------------
mkFullDeck :: Deck
mkFullDeck = Deck [Card r s | r <- [minBound ..], s <- [minBound ..]]

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------
rankToInt :: Rank -> Int
rankToInt r = fromEnum r + 1

-------------------------------------------------------------------------------
-- * Test functions
-------------------------------------------------------------------------------

-- | Generate a random `Suit`.
instance Arbitrary Suit where
  arbitrary :: Gen Suit
  arbitrary = elements [minBound :: Suit ..]


-- | Generate a random `Rank`.
instance Arbitrary Rank where
  arbitrary :: Gen Rank
  arbitrary = elements [minBound :: Rank ..]


-- | Generate a random `Card`.
instance Arbitrary Card where
  arbitrary :: Gen Card
  arbitrary = Card <$> arbitrary <*> arbitrary


