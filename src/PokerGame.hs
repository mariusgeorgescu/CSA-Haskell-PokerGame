{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PokerGame
  ( mkHand
  , startPokerGame
  , Hand(handCards)
  , setDealer
  , dealHands
  , PokerGame(players)
  , PokerPlayer(hand)
  , initPokerGame
  , addPlayerToGame
  , discardAndDraw
  ) where

import           Cards           (Card, Deck, drawCards, mkFullDeck,
                                  shuffleDeck)
import           Data.List       (intercalate, nub)

import           Data.Bifunctor  (Bifunctor (bimap))
import           Data.List.Extra (groupSort, partition)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------
-- | Player actions
data PokerPlayerAction
  = Bet
  | Fold
  | Call
  | Raise Int
  | AllIn Int
  deriving (Show)


-- | Game state
data GameState
  = NotStarted -- ^ The initial state of the game, where players are not yet playing.
  | ChooseDealer -- ^ Choose who is the dealer and determine the blinds based on that.
  | PostingBlinds -- ^ Before each hand, the two players to the left of the dealer are required to post blind bets, which are small forced bets that begin the pot.
  | DealingHands -- ^ After the blinds have been posted, the dealer deals five cards to each player, face down
  | FirstBetRound -- ^ Once the cards have been dealt, the first round of betting begins. Players can either fold, call (match the previous bet), or raise (increase the previous bet).
  | Drawing -- ^ After the first round of betting, players have the option to discard some or all of their cards and receive new ones from the dealer.
  | SecondBetRound -- ^ After the drawing round, another round of betting begins, with the same options as before: fold, call, or raise
  | Showdown -- ^ If more than one player is still in the hand after the second round of betting, a showdown occurs, where the players reveal their cards and the best hand wins the pot.
  | EndOfHand -- ^ After the pot has been awarded, the next hand begins, with the player to the left of the previous dealer becoming the new dealer.
  deriving (Show, Enum, Eq)


-- | Game
data PokerGame =
  FiveCardDraw
    { state           :: GameState
    , deck            :: Deck
    , muck            :: [Card]
    , bets            :: [(PokerPlayer, Int)]
    , dealerIndex     :: Maybe Int
    , playerTurnIndex :: Maybe Int
    , minBet          :: Int
    , players         :: [PokerPlayer]
    }


-- | Player
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


-- | Hand
newtype Hand =
  FiveCardDrawHand
    { handCards :: [Card]
    }
  deriving (Eq)

-------------------------------------------------------------------------------
--  Instances
-------------------------------------------------------------------------------
instance Show Hand where
  show :: Hand -> String
  show (FiveCardDrawHand cards) = show cards

instance Arbitrary Hand where
  arbitrary :: Gen Hand
  arbitrary = do
    cards <- vectorOf 5 (arbitrary :: Gen Card)
    case mkHand cards of
      Left _   -> arbitrary
      Right ha -> return ha

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

instance Eq PokerPlayer
 where
  (==) :: PokerPlayer -> PokerPlayer -> Bool
  (==) p1 p2 = identity p1 == identity p2

instance Ord PokerPlayer where
  compare :: PokerPlayer -> PokerPlayer -> Ordering
  compare p1 p2 = compare (identity p1) (identity p2)

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

-------------------------------------------------------------------------------
-- * Hand function
-------------------------------------------------------------------------------
{- | Given a list of cards exactly five cards, constructs a poket hand.
If the number of cards is not equal to five, or there are duplicate cards, mkHand returns Left. -}
mkHand :: [Card] -> Either String Hand
mkHand cards_
  | length cards_ /= 5       = Left "Hand: Wrong no. of cards"
  | length (nub cards_) /= 5 = Left "Hand: Contains duplicates"
  | otherwise                = Right $ FiveCardDrawHand cards_


-- | Given a list up to five cards, a list of indexes and a hand the function replaces the new cards.
updateHand :: [Card] -> [Int] -> Hand -> Either String ([Card], Hand)
updateHand new_cards cards_to_discard FiveCardDrawHand {handCards}
  | length new_cards > 5 = Left "Hand: Wrong no. of cards to replace"
  | length new_cards /= length cards_to_discard =
    Left "Hand: no. cards to discard different than no. of cards drawn"
  | length (nub (new_cards ++ handCards)) /= length (new_cards ++ handCards) =
    Left "Hand: Deck contains duplicates"
  | otherwise = do
    let (discarded, remaining) = splitByIndices cards_to_discard handCards
    new_hand <- mkHand (remaining ++ new_cards)
    return (discarded, new_hand)

-------------------------------------------------------------------------------
-- * Player functions
-------------------------------------------------------------------------------
{- | Given id, name and no. of chips, constructs a poker player.
Returns Left if the id or chips are < 0 or the name is empty.-}
mkPokerPlayer :: Int -> String -> Int -> Either String PokerPlayer
mkPokerPlayer id_ name_ chips_
  | chips_ < 0 = Left "Player: Invalid no. of chips"
  | id_ < 0    = Left "Player: Invalid ID"
  | null name_ = Left "Player: Invalid name"
  | otherwise =
    Right $ PokerPlayer False Nothing Nothing Nothing Nothing id_ name_ chips_

-------------------------------------------------------------------------------
-- * Game functions
-------------------------------------------------------------------------------
-- | Function to initate a PokerGame in NotStarted state, with a valid Deck and mininum bet value.
initPokerGame ::
     [Int] -- ^ Permutations for shuffling the deck
  -> Int -- ^ Minimum bet value
  -> Either String PokerGame
initPokerGame perms minBet_
  | minBet_ <= 0 = Left "Game: Invalid minimum bet value"
  | otherwise    = do
    deck <- shuffleDeck perms mkFullDeck
    Right $ FiveCardDraw NotStarted deck [] [] Nothing Nothing minBet_ []


-- | Function to add a player to a game. Returns a game with updated list of players
addPlayerToGame ::
     String -- ^ Name of the player
  -> Int -- ^ No. of chips
  -> PokerGame -- ^ The PokerGame to add the player
  -> Either String PokerGame
addPlayerToGame p_name p_chips game@FiveCardDraw {state, players, minBet}
  | state /= NotStarted = Left "Game: Invalid state for adding new player"
  | null p_name         = Left "Game: Invalid player name"
  | p_chips <= minBet   = Left "Game: Invalid no. of chips"
  | otherwise           = do
    new_player <- mkPokerPlayer (length players) p_name p_chips
    return $ game {players = new_player : players}


-- | Function to start a NotStarted game. Returns a game with updated state.
startPokerGame :: PokerGame -> Either String PokerGame
startPokerGame game@FiveCardDraw {state, players}
  | state /= NotStarted                      = Left "Game: Invalid state to start the game"
  | length players < 2 || length players > 5 = Left "Game: Wrong no. of players"
  | otherwise                                = Right $ game {state = ChooseDealer}


-- | Function to set the Dealer. Returns a game with updated state and playerTurnIndex
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


-- -- | Function for posting blinds. Returns a game with updated state, pot and players' data.
-- postBlinds :: PokerGame -> Either String PokerGame
-- postBlinds game@FiveCardDraw {..}
--   | state /= PostingBlinds = Left "Game: Wrong game state for posting blinds"
--   | otherwise = do
--     pti <- maybeToEither "invalid state" playerTurnIndex
--     let (b1, b2) = (players !! pti, players !! (pti + 1) )
--     let b1_ = b1{bet}

--     return _

-- | Function to deal hands to players. Returns a game with updated state, deck and players' data.
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
  where
    dealHandToPlayer :: PokerPlayer -> [Card] -> Either String PokerPlayer
    dealHandToPlayer player_ cards_ = do
      h <- mkHand cards_
      return $ player_ {hand = Just h}


-- | Function to discard and draw for a player. Returns a game with updated state, deck and the player data.
discardAndDraw :: Int -> [Int] -> PokerGame -> Either String PokerGame
discardAndDraw pid to_discard game@FiveCardDraw { state
                                                , players
                                                , playerTurnIndex
                                                , deck
                                                , muck
                                                }
  | state /= Drawing                 = Left "Game: Invalid state for drawing"
  | length to_discard > 5            = Left "Game: Invalid no of cards to discard"
  | not (isPlayerInGame pid players) = Left "Game: Invalid player id"
  | playerTurnIndex /= Just pid      = Left "Game: Invalid player turn"
  | otherwise                        = do
    let current_player = players !! pid
    case hand current_player of
      Nothing -> Left "Empty hand"
      Just current_hand -> do
        (drawn, new_deck) <- drawCards (length to_discard) deck
        (discarded, new_hand) <- updateHand drawn to_discard current_hand
        let new_player = current_player {hand = Just new_hand}
        let new_players = updateList players pid new_player
        let updated_game =
              game
                { muck = discarded ++ muck
                , deck = new_deck
                , players = new_players
                }
        return updated_game
  where
    isPlayerInGame :: Int -> [PokerPlayer] -> Bool
    isPlayerInGame i pls = i `elem` (identity <$> pls)


-- placeBet = _

-- drawNewCard = _

-- discardCard = _
-------------------------------------------------------------------------------
-- *  utilities
-------------------------------------------------------------------------------
splitByIndices :: (Foldable t, Eq a, Num a, Enum a) => t a -> [b] -> ([b], [b])
splitByIndices is xs =
  bimap (fmap snd) (fmap snd) $
  partition (\(i, _) -> i `elem` is) (zip [0 ..] xs)

updateList :: [a] -> Int -> a -> [a]
updateList xs index newVal = take index xs ++ [newVal] ++ drop (index + 1) xs
