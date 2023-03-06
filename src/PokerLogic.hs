{-# LANGUAGE InstanceSigs #-}

module PokerLogic where

import           Cards         (Card (getRank, getSuit), Rank (..), Suit)
import           Data.Function (on)
import           Data.List     (group, sort, sortBy)
import           PokerGame     (Hand (..))

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------
-- | Hand ranks.
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


-- | Poker combination.
data Combination =
  Combination
    { handRank  :: HandRank
    , structure :: [Rank] -- ^ card ranks that indicate combination strength
    }
  deriving (Eq, Show)

instance Ord Combination where
  compare :: Combination -> Combination -> Ordering
  compare (Combination hr1 str1) (Combination hr2 str2) =
    case compare hr1 hr2 of
      LT -> LT
      GT -> GT
      EQ ->
        let a = concat $ fst <$> groupRanks str1
            b = concat $ fst <$> groupRanks str2
         in compare a b

-------------------------------------------------------------------------------
-- * Determine Combinations
-------------------------------------------------------------------------------
evaluateHand :: Hand -> Combination
evaluateHand h = Combination hand_type (concat $ fst <$> groups)
  where
    cards          = getHandCards h
    (ranks, suits) = (,) <$> sort . fmap getRank <*> fmap getSuit $ cards
    groups         = groupRanks ranks
    no_groups      = length groups
    hand_type
      | no_groups == 5 =
        case (isFlush suits, isStraight ranks) of
          (True, True) ->
            if ranks == [Ace, Ten, Jack, Queen, King]
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

-- | Group ranks in card list and sort in descending order by length.
groupRanks :: [Rank] -> [([Rank], Int)]
groupRanks rs =
  let rank_groups = group $ sortBy (flip compare) rs
      groups_and_count = (\xs -> (xs, length xs)) <$> rank_groups
   in sortBy (flip compare `on` snd) groups_and_count


-- | Count amount of each rank in card list.
countRanks :: [Rank] -> [Int]
countRanks rs = snd <$> groupRanks rs


-- | Given a list of ranks, returns True if the list contains unique and consecutive rank values
isConsecutive :: [Rank] -> Bool
isConsecutive []         = False
isConsecutive list@(x:_) = sort list == [x ..]

-------------------------------------------------------------------------------
-- * Untility functions
-------------------------------------------------------------------------------

-- | Given a list of ranks, returns True if the list contains exactly 5 ranks which form a Straight
isStraight :: [Rank] -> Bool
isStraight [Ace, Ten, Jack, Queen, King] = True
isStraight ranks_ = isConsecutive ranks_ && length ranks_ == 5


-- | Given a list of suits, returns True if the list contains exactly 5 suits of the same type.
isFlush :: [Suit] -> Bool
isFlush suits = all (== head suits) suits && length suits == 5


-- | Given a list of cards, returns True if the list contains exactly 5 cards of the same suit which form a StraightFlush.
isStraightFlush :: [Card] -> Bool
isStraightFlush cards =
  isFlush (getSuit <$> cards) && isConsecutive (getRank <$> cards)


-- | Given a list of cards, returns True if the list contains exactly 5 cards which form a RoyalFlush.
isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards =
  sort (getRank <$> cards) == [Ace, Ten, Jack, Queen, King] &&
  isFlush (getSuit <$> cards)
