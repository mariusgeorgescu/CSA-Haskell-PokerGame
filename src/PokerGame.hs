{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE NamedFieldPuns #-}

module PokerGame
  ( mkPokerPlayer
  , mkHand
  , startPokerGame
  , Hand(getHandCards)
  , setDealer
  , dealHands
  , PokerGame
  ) where

import           Cards           (Card, Deck, drawCards, mkFullDeck)
import           Data.List       (nub)

import           Data.List.Extra (groupSort)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------
data PokerPlayerAction
  = Bet
  | Fold
  | Call
  | Raise Int
  | AllIn Int
  deriving (Show)

data GameState
  = ChooseDealer -- ^ Choose who is the dealer and determine the blinds based on that.
  | PostingBlinds -- ^ Before each hand, the two players to the left of the dealer are required to post blind bets, which are small forced bets that begin the pot.
  | DealingHands -- ^ After the blinds have been posted, the dealer deals five cards to each player, face down
  | FirstBetRound -- ^ Once the cards have been dealt, the first round of betting begins. Players can either fold, call (match the previous bet), or raise (increase the previous bet).
  | Drawing -- ^ After the first round of betting, players have the option to discard some or all of their cards and receive new ones from the dealer.
  | SecondBetRound -- ^ After the drawing round, another round of betting begins, with the same options as before: fold, call, or raise
  | Showdown -- ^ If more than one player is still in the hand after the second round of betting, a showdown occurs, where the players reveal their cards and the best hand wins the pot.
  | EndOfHand -- ^ After the pot has been awarded, the next hand begins, with the player to the left of the previous dealer becoming the new dealer.
  deriving (Show, Enum, Eq)

data PokerGame =
  FiveCardDraw
    { state           :: GameState
    , deck            :: Deck
    , muck            :: [Card]
    , bets            :: [Int]
    , dealerIndex     :: Maybe Int
    , playerTurnIndex :: Maybe Int
    , minBet          :: Int
    , players         :: [PokerPlayer]
    }
  deriving (Show)

data PokerPlayer =
  PokerPlayer
    { acted      :: Bool
    , lastAction :: Maybe PokerPlayerAction
    , bet        :: Maybe Int
    , lastBet    :: Maybe Int
    , hand       :: Maybe Hand
    , identity   :: Int
    , name       :: String
    , chips      :: Int
    }
  deriving (Show)

instance Eq PokerPlayer
 where
  (==) :: PokerPlayer -> PokerPlayer -> Bool
  (==) p1 p2 = identity p1 == identity p2

instance Ord PokerPlayer where
  compare :: PokerPlayer -> PokerPlayer -> Ordering
  compare p1 p2 = compare (identity p1) (identity p2)

newtype Hand =
  FiveCardDrawHand
    { getHandCards :: [Card]
    }
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------
mkHand :: [Card] -> Either String Hand
mkHand cards_
  | length cards_ /= 5       = Left "Wrong no. of cards"
  | length (nub cards_) /= 5 = Left "Hand contains duplicates"
  | otherwise                = Right $ FiveCardDrawHand cards_

mkPokerPlayer :: Int -> String -> Int -> Either String PokerPlayer
mkPokerPlayer id_ name_ chips_
  | chips_ < 0 = Left "Invalid no. of chips"
  | id_ < 0    = Left "Invalid ID"
  | null name_ = Left "Invalid name"
  | otherwise =
    Right $ PokerPlayer False Nothing Nothing Nothing Nothing id_ name_ chips_

startPokerGame :: Int -> [PokerPlayer] -> Either String PokerGame
startPokerGame minBet_ players_
  | length players_ < 2 || length players_ > 5 = Left "Wrong no. of players"
  | minBet_ <= 0                               = Left "Invalid minimum bet value"
  | otherwise =
    Right $
    FiveCardDraw
      ChooseDealer
      mkFullDeck
      []
      (replicate (length players_) 0)
      Nothing
      Nothing
      minBet_
      players_

-------------------------------------------------------------------------------
-- * Game Logic Functions
-------------------------------------------------------------------------------
setDealer :: Int -> PokerGame -> Either String PokerGame
setDealer n game@FiveCardDraw {state}
  | state /= ChooseDealer = Left "Wrong game state"
  | n < 0                 = Left "Wrong dealer index"
  | otherwise =
    Right $
    game
      { dealerIndex = Just n
      , playerTurnIndex = Just (n + 1)
      , state = DealingHands
      }

dealHands :: PokerGame -> Either String PokerGame
dealHands game@FiveCardDraw {state, deck, players}
  | state /= DealingHands = Left "Invalid game state for dealing hands"
  | otherwise             = do
    let no_of_cards_to_deal = length players * 5
    (cards_to_deal, newDeck) <- drawCards no_of_cards_to_deal deck
    let deal = groupSort (zip (cycle players) cards_to_deal) -- deal in round robin fashion
    updatedPlayers <- traverse (uncurry dealHandToPlayer) deal
    return $
      game {state = FirstBetRound, deck = newDeck, players = updatedPlayers}

dealHandToPlayer :: PokerPlayer -> [Card] -> Either String PokerPlayer
dealHandToPlayer player_ cards_ = do
  h <- mkHand cards_
  return $ player_ {hand = Just h}

discardAndDraw :: Int -> Int -> PokerGame -> Either String PokerGame
discardAndDraw pid no_discard game@FiveCardDraw {state, players}
  | state /= Drawing                 = Left "Invalid state for drawing"
  | no_discard < 0                   = Left "Invalid no of cards to discard"
  | not (isPlayerInGame pid players) = Left "Invalid player id"
  | otherwise                        = Right game -- TODO

isPlayerInGame :: Int -> [PokerPlayer] -> Bool
isPlayerInGame i pls = i `elem` (identity <$> pls)


-- placeBet = _

-- drawNewCard = _

-- discardCard = _
-------------------------------------------------------------------------------
-- * Testing utilities
-------------------------------------------------------------------------------
instance Arbitrary Hand where
  arbitrary :: Gen Hand
  arbitrary = do
    cards <- vectorOf 5 (arbitrary :: Gen Card)
    case mkHand cards of
      Left _   -> arbitrary
      Right ha -> return ha
