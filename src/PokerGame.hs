{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PokerGame
  ( mkHand
  , startPokerGame
  , Hand(getHandCards)
  , setDealer
  , dealHands
  , PokerGame(players)
  , PokerPlayer(hand)
  , initPokerGame
  , addPlayerToGame
  ) where

import           Cards           (Card, Deck, drawCards, mkFullDeck,
                                  shuffleDeck)
import           Data.List       (intercalate, nub)

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
  = NotStarted
  | ChooseDealer -- ^ Choose who is the dealer and determine the blinds based on that.
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

data PokerPlayer =
  PokerPlayer
    { acted      :: Bool
    , lastAction :: Maybe PokerPlayerAction
    , hand       :: Maybe Hand
    , bet        :: Maybe Int
    , lastBet    :: Maybe Int
    , identity   :: Int
    , name       :: String
    , chips      :: Int
    }

newtype Hand =
  FiveCardDrawHand
    { getHandCards :: [Card]
    }
  deriving (Eq)

-------------------------------------------------------------------------------
-- * Instances
-------------------------------------------------------------------------------
instance Show Hand where
  show :: Hand -> String
  show (FiveCardDrawHand cards) = show cards

instance Show PokerPlayer where
  show :: PokerPlayer -> String
  show PokerPlayer {..} =
    "\tPLAYER #" ++
    show identity ++
    " " ++
    name ++
    "\n" ++
    "\tHand: " ++
    maybe "empty hand" show hand ++
    "\n" ++
    "\tChips: " ++
    show chips ++
    "\n" ++ "\tLast Action: " ++ maybe "no last action" show lastAction ++ "\n"

instance Show PokerGame where
  show :: PokerGame -> String
  show FiveCardDraw {..} =
    "------------------------------------\n" ++
    "------| POKER GAME DETAILS |------- \n" ++
    "------------------------------------\n" ++
    "Current state : " ++
    show state ++
    "\n" ++
    "Minimum bet: " ++
    show minBet ++
    "\n" ++
    "Dealer: " ++
    maybe "not set" (name . (players !!)) dealerIndex ++
    "\n" ++
    "Player turn: " ++
    maybe "not set" (name . (players !!)) playerTurnIndex ++
    "\n" ++
    "Deck :" ++
    show deck ++
    "\n" ++
    "Muck :" ++
    show muck ++
    "\n" ++
    "Bets :" ++
    show bets ++
    "\n" ++
    "Players :\n" ++
    "------------------------------------\n" ++
    intercalate "\n" (show <$> players) ++
    "------------------------------------"

instance Eq PokerPlayer
 where
  (==) :: PokerPlayer -> PokerPlayer -> Bool
  (==) p1 p2 = identity p1 == identity p2

instance Ord PokerPlayer where
  compare :: PokerPlayer -> PokerPlayer -> Ordering
  compare p1 p2 = compare (identity p1) (identity p2)

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------
mkHand :: [Card] -> Either String Hand
mkHand cards_
  | length cards_ /= 5       = Left "Hand: Wrong no. of cards"
  | length (nub cards_) /= 5 = Left "Hand: Contains duplicates"
  | otherwise                = Right $ FiveCardDrawHand cards_

mkPokerPlayer :: Int -> String -> Int -> Either String PokerPlayer
mkPokerPlayer id_ name_ chips_
  | chips_ < 0 = Left "Player: Invalid no. of chips"
  | id_ < 0    = Left "Player: Invalid ID"
  | null name_ = Left "Player: Invalid name"
  | otherwise =
    Right $ PokerPlayer False Nothing Nothing Nothing Nothing id_ name_ chips_

initPokerGame :: [Int] -> Int -> Either String PokerGame
initPokerGame perms minBet_
  | minBet_ <= 0 = Left "Game: Invalid minimum bet value"
  | otherwise    = do
    deck <- shuffleDeck perms mkFullDeck
    Right $ FiveCardDraw NotStarted deck [] [] Nothing Nothing minBet_ []

addPlayerToGame :: String -> Int -> PokerGame -> Either String PokerGame
addPlayerToGame p_name p_chips game@FiveCardDraw {state, players, minBet}
  | state /= NotStarted = Left "Game: Invalid state for adding new player"
  | null p_name         = Left "Game: Invalid player name"
  | p_chips <= minBet   = Left "Game: Invalid no. of chips"
  | otherwise           = do
    new_player <- mkPokerPlayer (length players) p_name p_chips
    return $ game {players = new_player : players}

startPokerGame :: PokerGame -> Either String PokerGame
startPokerGame game@FiveCardDraw {state, players}
  | state /= NotStarted                      = Left "Game: Invalid state to start the game"
  | length players < 2 || length players > 5 = Left "Game: Wrong no. of players"
  | otherwise                                = Right $ game {state = ChooseDealer}

-------------------------------------------------------------------------------
-- * Game Logic Functions
-------------------------------------------------------------------------------
setDealer :: Int -> PokerGame -> Either String PokerGame
setDealer n game@FiveCardDraw {state, players}
  | state /= ChooseDealer = Left "Game: Wrong game state to set dealer"
  | n < 0                 = Left "Game: Wrong dealer index"
  | otherwise =
    Right $
    game
      { dealerIndex = Just n
      , playerTurnIndex = Just ((n + 1) `mod` length players)
      , state = DealingHands
      }

dealHands :: PokerGame -> Either String PokerGame
dealHands game@FiveCardDraw {state, deck, players}
  | state /= DealingHands = Left "Game: Invalid game state for dealing hands"
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
  | state /= Drawing                 = Left "Game: Invalid state for drawing"
  | no_discard < 0                   = Left "Game: Invalid no of cards to discard"
  | not (isPlayerInGame pid players) = Left "Game: Invalid player id"
  | otherwise = Right game -- TODO
  where
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
