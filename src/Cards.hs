{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module Cards
  ( Suit
  , Rank(..)
  , Card(getRank, getSuit)
  , Deck(getCards)
  , mkFullDeck
  , rankToInt
  , shuffleDeckR
  , shuffleDeck
  , intToRank
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.String            (IsString (fromString))
import           GHC.StaticPtr          (IsStatic)
import           System.Random          (newStdGen)
import           System.Random.Shuffle  as SRS (shuffle, shuffle')
import           Test.QuickCheck        (Arbitrary (arbitrary), Gen, elements)

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


-- | Card type.
data Card =
  Card
    { getRank :: !Rank
    , getSuit :: !Suit
    }
  deriving (Eq)


-- | Deck type.
newtype Deck =
  Deck
    { getCards :: [Card]
    }
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Instances
-------------------------------------------------------------------------------
instance IsString Rank where
  fromString :: String -> Rank
  fromString str =
    case str of
      "A" -> Ace
      "K" -> King
      "Q" -> Queen
      "J" -> Jack
      _   -> intToRank (read str)

instance Ord Card
 where
  (<=) :: Card -> Card -> Bool
  c1 <= c2 = getRank c1 <= getRank c2

instance Show Card where
  show :: Card -> String
  show (Card r s) = show r ++ " of " ++ show s

instance IsString Card where
  fromString :: String -> Card
  fromString str =
    let s = last str
        r = init str
     in Card (fromString r) (charToSuit s)

-------------------------------------------------------------------------------
-- * Deck operation functions
-------------------------------------------------------------------------------
mkFullDeck :: Deck
mkFullDeck = Deck [Card r s | r <- [minBound ..], s <- [minBound ..]]

shuffleDeck :: [Int] -> Deck -> Either String Deck
shuffleDeck permutations (Deck cards)
  | length permutations == 51 = Right $ Deck $ SRS.shuffle cards permutations
  | otherwise                 = Left "Invalid permutations"

shuffleDeckR :: MonadIO m => Deck -> m Deck
shuffleDeckR (Deck cards) = do
  Deck . SRS.shuffle' cards (length cards) <$> newStdGen

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------
rankToInt :: Rank -> Int
rankToInt r = fromEnum r + 1

intToRank :: Int -> Rank
intToRank i
  | i <= 13 && i >= 1 = toEnum $ i - 1
  | otherwise         = error "value must be between 1 and 13 "


-- | Converts char to suit
charToSuit :: Char -> Suit
charToSuit =
  \case
    'h' -> Hearts
    'c' -> Clubs
    'd' -> Diamonds
    's' -> Spades
    _   -> error "wrong char"

-------------------------------------------------------------------------------
-- * Testing utilities
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
