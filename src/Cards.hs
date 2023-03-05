{-# LANGUAGE InstanceSigs #-}

module Cards
  ( Suit
  , Rank(..)
  , Card(getRank, getSuit)
  , Deck
  , mkFullDeck
  , rankToInt
  ) where

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
  deriving (Show, Eq)

instance Ord Card
 where
  (<=) :: Card -> Card -> Bool
  c1 <= c2 = getRank c1 <= getRank c2


-- instance Show Card where
--   show :: Card -> String
--   show (Card r s) = show r ++ " of " ++ show s
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
