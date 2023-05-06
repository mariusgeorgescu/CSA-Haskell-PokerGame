{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module Cards
  ( mkFullDeck
  , shuffleDeck
  , shuffleDeckR
  , drawCards
  , Card(cardRank, cardSuit)
  , Rank(..)
  , Suit(..)
  , Deck(deckCards)
  , rankToInt
  , intToRank
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Binary
import           Data.Function          (on)
import           Data.String            (IsString (fromString))
import           GHC.Generics           (Generic)
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
  deriving (Eq, Enum, Bounded, Generic)

-- | Card ranks.
data Rank
  = Two
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
  | Ace
  deriving (Eq, Ord, Enum, Bounded, Generic)

-- | Card type.
data Card =
  Card
    { cardRank :: !Rank
    , cardSuit :: !Suit
    }
  deriving (Eq, Generic)

-- | Deck type.
newtype Deck =
  Deck
    { deckCards :: [Card]
    }
  deriving (Eq)

-------------------------------------------------------------------------------
--  Instances
-------------------------------------------------------------------------------
instance Binary Suit

instance Binary Rank

instance Binary Card

instance Show Suit where
  show :: Suit -> String
  show Spades   = "\x2660"
  show Hearts   = "\x1b[31m\x2665\x1b[0m"
  show Diamonds = "\x1b[31m\x2666\x1b[0m"
  show Clubs    = "\x2663"

instance Show Rank where
  show :: Rank -> String
  show r =
    case r of
      Ace   -> "A"
      King  -> "K"
      Queen -> "Q"
      Jack  -> "J"
      _     -> show . rankToInt $ r

instance IsString Rank where
  fromString :: String -> Rank
  fromString str =
    case str of
      "A" -> Ace
      "K" -> King
      "Q" -> Queen
      "J" -> Jack
      _   -> intToRank (read str)

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare = compare `on` cardRank

instance Show Card where
  show :: Card -> String
  show (Card rank suit) = show rank ++ " of " ++ show suit ++ "  "

instance IsString Card where
  fromString :: String -> Card
  fromString string =
    let suit = last string
        rank = init string
     in Card (fromString rank) (unsafeCharToSuit suit)

instance Show Deck where
  show :: Deck -> String
  show = show . deckCards

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

-------------------------------------------------------------------------------
-- * Functions on Deck
-------------------------------------------------------------------------------
-- | Function to build a full deck of cards
mkFullDeck :: Deck
mkFullDeck = Deck [Card r s | r <- [minBound ..], s <- [minBound ..]]

-- | Given a list of permutations this function return a shuffled deck.
shuffleDeck :: [Int] -> Deck -> Either String Deck
shuffleDeck permutations (Deck cards)
  | length permutations == 51 = Right $ Deck $ SRS.shuffle cards permutations
  | otherwise                 = Left "Invalid permutations"

-- | This function return a shuffled deck using newStdGen
shuffleDeckR :: (MonadIO m) => Deck -> m Deck
shuffleDeckR (Deck cards) = do
  Deck . SRS.shuffle' cards (length cards) <$> newStdGen

-- | Function to draw cards from a deck of cards
-- Returns the first n cards and the updated deck
drawCards :: Int -> Deck -> Either String ([Card], Deck)
drawCards n (Deck cards)
  | n >= 0    = Right (take n cards, Deck (drop n cards))
  | otherwise = Left "No. of cards to draw must be positive"

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------
-- | Converts Rank to Int
rankToInt :: Rank -> Int
rankToInt r = fromEnum r + 2

-- | Converts Int to Rank
intToRank :: Int -> Rank
intToRank i = toEnum $ (i `mod` 13) - 2

-- | Converts char to suit
unsafeCharToSuit :: Char -> Suit
unsafeCharToSuit =
  \case
    'h' -> Hearts
    'c' -> Clubs
    'd' -> Diamonds
    's' -> Spades
    _   -> error "wrong char"
