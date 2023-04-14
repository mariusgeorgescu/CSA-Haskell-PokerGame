{-# LANGUAGE InstanceSigs #-}

module PokerLogic where

import           Cards         (Card (..), Rank (..), Suit)
import           Data.Function (on)
import           Data.List     (group, sort, sortBy)


-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------
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

{- | A @Combination@ is defined by the @HandRank@ and it's structure that indicate
the combination strength. -}
data Combination =
  Combination
    { combHandRank  :: HandRank -- ^ rank of the hand
    , combStructure :: [Rank] -- ^ card ranks that indicate combination strength
    }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
--  Instances
-------------------------------------------------------------------------------
instance Ord Combination where
  compare :: Combination -> Combination -> Ordering
  compare c1 c2 =
    case (compare `on` combHandRank) c1 c2 of
      LT -> LT
      GT -> GT
      EQ -> (compare `on` combStructure) c1 c2

-------------------------------------------------------------------------------
-- * Evaluate hand
-------------------------------------------------------------------------------
-- | This function evaluates a hand to determine the highest combination
evaluateHand :: [Card] -> Combination
evaluateHand cards = Combination hand_type (concatMap fst groups)
  where
    (ranks, suits) = (,) <$> sort . fmap cardRank <*> fmap cardSuit $ cards
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

-- | Function that groups a list of @Rank@ and sorts  it in descending order by group length.
groupRanks :: [Rank] -> [([Rank], Int)]
groupRanks ranks =
  let rank_groups = group $ sortBy (flip compare) ranks
      groups_and_count = (\xs -> (xs, length xs)) <$> rank_groups
   in sortBy (flip compare `on` snd) groups_and_count


-- | Given @[Rank]@, returns @True@ if the list contains unique and consecutive @Rank@ values.
isConsecutive :: [Rank] -> Bool
isConsecutive []         = False
isConsecutive list@(x:_) = sort list == take 5 [x ..]

-------------------------------------------------------------------------------
-- * Verify Hand Type Functions
-------------------------------------------------------------------------------

-- | Given a @[Rank]@, returns @True@ if the list contains exactly 5 ranks which form a @Straight@.
isStraight :: [Rank] -> Bool
isStraight [Ace, Ten, Jack, Queen, King] = True
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
  sort (cardRank <$> cards) == [Ace, Ten, Jack, Queen, King] &&
  isFlush (cardSuit <$> cards)
