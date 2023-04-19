{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module PokerLogic where

import Cards (Card (..), Rank (..), Suit)
import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.Validation (Validation (..), toEither)
import Test.QuickCheck (Arbitrary, Gen, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Utilities (hasDuplicates, splitByIndices)

-------------------------------------------------------------------------------

-- * Declarations

-------------------------------------------------------------------------------

-- | Hand
newtype Hand = PokerHand
  { handCards :: [Card]
  }
  deriving (Eq)

-- | Hand ranks in Poker Game.
data HandRank
  = Highcard
  | OnePair
  | TwoPairs
  | ThreeOfaKind
  | Straight
  | Flush
  | FullHouse
  | FourOfaKind
  | StraightFlush
  | RoyalFlush
  deriving (Eq, Ord, Show)

-- | A @Combination@ is defined by the @HandRank@ and it's structure that indicate
-- the combination strength.
data Combination = Combination
  { -- | rank of the hand
    combHandRank :: HandRank,
    -- | card ranks that indicate combination strength
    combStructure :: [Rank]
  }
  deriving (Eq)

instance Show Combination where
  show :: Combination -> String
  show Combination {..} = show combHandRank ++ " " ++ show combStructure

---------------------------------------
----------------------------------------
--  Instances
-------------------------------------------------------------------------------

instance Show Hand where
  show :: Hand -> String
  show (PokerHand cards) = show cards

instance Arbitrary Hand where
  arbitrary :: Gen Hand
  arbitrary = do
    cards <- vectorOf 5 (arbitrary :: Gen Card)
    case mkHand cards of
      Left _ -> arbitrary
      Right ha -> return ha

instance Ord Combination where
  compare :: Combination -> Combination -> Ordering
  compare c1 c2 =
    case (compare `on` combHandRank) c1 c2 of
      LT -> LT
      GT -> GT
      EQ -> (compare `on` combStructure) c1 c2

-------------------------------------------------------------------------------

-- * Hand function

-------------------------------------------------------------------------------

-- | Given a list of cards exactly five cards, constructs a poket hand.
-- If the number of cards is not equal to five, or there are duplicate cards, mkHand returns Left.
mkHand :: [Card] -> Either String Hand
mkHand cards = toEither $ PokerHand <$> validateHandCards cards

validateHandCards :: [Card] -> Validation String [Card]
validateHandCards cards
  | length cards /= 5 = Failure "| Hand: Wrong no. of cards"
  | hasDuplicates cards = Failure "| Hand: Contains duplicates"
  | otherwise = Success cards

-- | Given a list up to five cards, a list of indexes and a hand the function replaces the new cards.
updateHand :: [Card] -> [Int] -> Hand -> Either String ([Card], Hand)
updateHand new_cards cards_to_discard PokerHand {handCards}
  | length new_cards > 5 = Left "Hand: Wrong no. of cards to replace"
  | length new_cards /= length cards_to_discard =
      Left "Hand: no. cards to discard different than no. of cards drawn"
  | hasDuplicates (new_cards ++ handCards) =
      Left "Hand: Deck contains duplicates"
  | otherwise = do
      let (discarded, remaining) = splitByIndices cards_to_discard handCards
      new_hand <- mkHand (remaining ++ new_cards)
      return (discarded, new_hand)

-------------------------------------------------------------------------------

-- * Evaluate hand

-------------------------------------------------------------------------------

-- | This function evaluates a hand to determine the highest combination
evaluateHand :: [Card] -> Combination
evaluateHand cards = Combination hand_type (concatMap fst groups)
  where
    (ranks, suits) = (,) <$> sort . fmap cardRank <*> fmap cardSuit $ cards
    groups = groupRanks ranks
    no_groups = length groups
    hand_type
      | no_groups == 5 =
          case (isFlush suits, isStraight ranks) of
            (True, True) ->
              if ranks == [Ten, Jack, Queen, King, Ace]
                then RoyalFlush
                else StraightFlush
            (True, False) -> Flush
            (False, True) -> Straight
            (False, False) -> Highcard
      | no_groups == 4 = OnePair
      | no_groups == 3 =
          if snd (head groups) == 3
            then ThreeOfaKind
            else TwoPairs
      | no_groups == 2 =
          if snd (head groups) == 4
            then FourOfaKind
            else FullHouse
      | otherwise = error "Invalid Hand"

-------------------------------------------------------------------------------

-- * Utility functions

-------------------------------------------------------------------------------

-- | Function that groups a list of @Rank@ and sorts  it in descending order by group length.
groupRanks :: [Rank] -> [([Rank], Int)]
groupRanks ranks =
  let rank_groups = group $ sortBy (flip compare) ranks
      groups_and_count = (\xs -> (xs, length xs)) <$> rank_groups
   in sortBy (flip compare `on` snd) groups_and_count

-- | Given @[Rank]@, returns @True@ if the list contains unique and consecutive @Rank@ values.
isConsecutive :: [Rank] -> Bool
isConsecutive [] = False
isConsecutive list@(x : _) = sort list == take 5 [x ..]

-------------------------------------------------------------------------------

-- * Verify Hand Type Functions

-------------------------------------------------------------------------------

-- | Given a @[Rank]@, returns @True@ if the list contains exactly 5 ranks which form a @Straight@.
isStraight :: [Rank] -> Bool
isStraight [Ten, Jack, Queen, King, Ace] = True
isStraight ranks_ = isConsecutive ranks_ && length ranks_ == 5

-- | Given a @[Suit]@, returns True if the list contains exactly 5 suits of the same type.
isFlush :: [Suit] -> Bool
isFlush suits = all (== head suits) suits && length suits == 5

-- | Given a @[Card]@, returns @True@ if the list contains exactly 5 cards of the same suit which form a @StraightFlush@.
isStraightFlush :: [Card] -> Bool
isStraightFlush cards =
  isFlush (cardSuit <$> cards) && isConsecutive (cardRank <$> cards)

-- | Given a @[Card]@, returns @True@ if the list contains exactly 5 cards which form a @RoyalFlush@.
isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards =
  sort (cardRank <$> cards) == [Ten, Jack, Queen, King, Ace]
    && isFlush (cardSuit <$> cards)
