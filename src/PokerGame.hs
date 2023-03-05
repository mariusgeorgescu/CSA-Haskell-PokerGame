module PokerGame where

import           Cards (Card, Deck, mkFullDeck)

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
  deriving (Show, Enum)

data PokerGame =
  FiveCardDraw
    { state           :: GameState
    , deck            :: Deck
    , muck            :: [Card]
    , bets            :: [Int]
    , dealerIndex     :: Maybe Int
    , playerTurnIndex :: Int
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
    , name       :: String
    , chips      :: Int
    }
  deriving (Show)

newtype Hand =
  FiveCardDrawHand
    { getHandCards :: [Card]
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------
mkHand :: [Card] -> Either String Hand
mkHand cs
  | length cs /= 5 = Left "Wrong no. of cards"
  | otherwise      = Right $ FiveCardDrawHand cs

mkPokerPlayer :: String -> Int -> PokerPlayer
mkPokerPlayer = PokerPlayer False Nothing Nothing Nothing Nothing

startPokerGame :: Int -> [PokerPlayer] -> Either String PokerGame
startPokerGame mb ps
  | length ps < 2 || length ps > 5 && mb > 0 = Left "Wrong no. of players"
  | otherwise =
    Right $
    FiveCardDraw
      ChooseDealer
      mkFullDeck
      []
      (replicate (length ps) 0)
      Nothing
      0
      mb
      ps

-- setDealer :: Int -> PokerGame -> PokerPlayer
-- setDealer n game = _

-- placeBet = _

-- drawNewCard = _

-- discardCard = _
