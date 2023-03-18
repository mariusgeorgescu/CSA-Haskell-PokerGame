{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module PokerGame
  ( mkHand
  , startPokerGame
  , Hand(handCards)
  , setDealer
  , dealHands
  , PokerGame(_gamePlayers, _gamePlayerTurnIndex)
  , PokerPlayer(_playerHand)
  , PokerPlayerAction(..)
  , initPokerGame
  , addPlayerToGame
  , discardAndDraw
  , postBlinds
  , inGameBettingAction
  ) where

import           Cards             (Card, Deck, drawCards, mkFullDeck,
                                    shuffleDeck)
import           Control.Lens

import           Data.Either.Extra (maybeToEither)
import           Data.List         (intercalate, nub)
import           Data.List.Extra   (groupSort, partition)
import           Data.Maybe        (isNothing)
import           Data.Validation
import           Test.QuickCheck   (Arbitrary (arbitrary), Gen, vectorOf)

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------
-- | Player actions
data PokerPlayerAction
  = Bet Int
  | FoldHand
  | Call Int
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
    { _gameState           :: GameState
    , _gameDeck            :: Deck
    , _gameMuck            :: [Card]
    , _gameBets            :: [(Int, PokerPlayerAction)]
    , _gameDealerIndex     :: Maybe Int
    , _gamePlayerTurnIndex :: Maybe Int
    , _gameMinBet          :: Int
    , _gamePlayers         :: [PokerPlayer]
    }


-- | Player
data PokerPlayer =
  PokerPlayer
    { _playerBettingActions :: [PokerPlayerAction]
    , _playerHand           :: Maybe Hand
    , _playerId             :: Int
    , _playerName           :: String
    , _playerChips          :: Int
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

-- Generate lenses for PokerGame
makeLenses ''PokerGame


-- Generate lenses for PokerPlayer
makeLenses ''PokerPlayer

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
    "\tPLAYER #" ++ show _playerId ++ " " ++ _playerName ++ "\n" ++ "\tHand: " ++
    maybe "empty hand" show _playerHand ++
    "\n" ++
    "\tChips: " ++
    show _playerChips ++
    "\n" ++
    "\tLast Action: " ++
    if null _playerBettingActions
      then "no last action"
      else show _playerBettingActions ++ "\n"

instance Eq PokerPlayer
 where
  (==) :: PokerPlayer -> PokerPlayer -> Bool
  (==) p1 p2 = _playerId p1 == _playerId p2

instance Ord PokerPlayer where
  compare :: PokerPlayer -> PokerPlayer -> Ordering
  compare p1 p2 = compare (_playerId p1) (_playerId p2)

instance Show PokerGame where
  show :: PokerGame -> String
  show FiveCardDraw {..} =
    "------------------------------------\n" ++
    "------| POKER GAME DETAILS |------- \n" ++
    "------------------------------------\n" ++
    "Current state : " ++
    show _gameState ++
    "\n" ++
    "Minimum bet: " ++
    show _gameMinBet ++
    "\n" ++
    "Dealer: " ++
    maybe "not set" (_playerName . (_gamePlayers !!)) _gameDealerIndex ++
    "\n" ++
    "Player turn: " ++
    maybe "not set" (_playerName . (_gamePlayers !!)) _gamePlayerTurnIndex ++
    "\n" ++
    "Deck :" ++
    show _gameDeck ++
    "\n" ++
    "Muck :" ++
    show _gameMuck ++
    "\n" ++
    "Bets :" ++
    show _gameBets ++
    "\n" ++
    "Players :\n" ++
    "------------------------------------\n" ++
    intercalate "\n" (show <$> _gamePlayers) ++ "\n" ++
    "------------------------------------"

-------------------------------------------------------------------------------
-- * Hand function
-------------------------------------------------------------------------------
{- | Given a list of cards exactly five cards, constructs a poket hand.
If the number of cards is not equal to five, or there are duplicate cards, mkHand returns Left. -}
mkHand :: [Card] -> Either String Hand
mkHand cards = toEither $ FiveCardDrawHand <$> validateHandCards cards

validateHandCards :: [Card] -> Validation String [Card]
validateHandCards cards
  | length cards /= 5       = Failure "| Hand: Wrong no. of cards"
  | length (nub cards) /= 5 = Failure "| Hand: Contains duplicates"
  | otherwise               = Success cards


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
mkPokerPlayer = ((toEither .) .) . mkPokerPlayer'

mkPokerPlayer' :: Int -> String -> Int -> Validation String PokerPlayer
mkPokerPlayer' p_id p_name p_chips =
  PokerPlayer <$> Success [] <*> Success Nothing <*> valid_id <*> valid_name <*>
  valid_chips
  where
    valid_chips =
      if p_chips < 0
        then Failure "| Player: Invalid no. of chips"
        else Success p_chips
    valid_id =
      if p_id < 0
        then Failure "| Player: Invalid ID"
        else Success p_id
    valid_name =
      if null p_name
        then Failure "| Player: Invalid name"
        else Success p_name


--       setNextPlayerTurn

--     put $ game & gamePlayers . ix (player ^. playerId) .~ updated_player &

--     updated_player <- player & playerBettingAction (Call val_to_call)

--   else do

--   then throwError "Game: Insufficient chips to call"

-- if val_to_call > player ^. playerChips

-- let val_to_call = game ^. gameMinBet - player_stake

-- let player_stake = sum $ actionVal <$> (player ^. playerBettingActions)

-- let player = (game ^. gamePlayers) !! pid

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


-- | Function to add a player to a game.
-- Returns a game with updated list of players.
addPlayerToGame ::
     String -- ^ Name of the player
  -> Int -- ^ No. of chips
  -> PokerGame -- ^ The PokerGame to add the player
  -> Either String PokerGame
addPlayerToGame p_name p_chips game
  | null p_name                   = Left "Game: Invalid player name"
  | p_chips <= game ^. gameMinBet = Left "Game: Invalid no. of chips"
  | otherwise                     = do
    toEither . isValidForAddPlayer $ game
    new_player <- mkPokerPlayer (length $ game ^. gamePlayers) p_name p_chips
    return $ game & gamePlayers %~ (++[new_player])


-- | Function to check if a game is in a valid state to add a new player.
isValidForAddPlayer :: PokerGame -> Validation String ()
isValidForAddPlayer FiveCardDraw {..}
  | _gameState /= NotStarted =
    Failure "Game: Invalid state for adding new player"
  | length _gamePlayers >= 5 = Failure "Game: To many players"
  | otherwise                = Success ()


-- | Function to start a poker game.
-- Returns a game with updated gameState.
startPokerGame :: PokerGame -> Either String PokerGame
startPokerGame game = do
  toEither . isValidForStart $ game
  return $ game & gameState .~ ChooseDealer


-- | Function to check if a game is in a valid state to start.
isValidForStart :: PokerGame -> Validation String ()
isValidForStart FiveCardDraw {..}
  | _gameState /= NotStarted = Failure "Game: Invalid state to start the game"
  | ((||) <$> (< 2) <*> (> 5)) . length $ _gamePlayers =
    Failure "Game: Wrong no. of players"
  | otherwise = Success ()


-- | Function to set the dealer of a poker game.
-- Returns a game with updated  dealerIndex, playerTurnIndex and gameState
setDealer :: Int -> PokerGame -> Either String PokerGame
setDealer n game@FiveCardDraw {..}
  | n < 0                         = Left "Game: Dealer index must be positive"
  | n > (length _gamePlayers - 1) = Left "Game: Dealer index to high"
  | otherwise                     = do
    toEither . isValidForChooseDealer $ game
    return $ game & gameDealerIndex ?~ n
                  & gamePlayerTurnIndex ?~ ((n + 1) `mod` length (game ^. gamePlayers))
                  & gameState .~ PostingBlinds

-- | Function to check if a game is in a valid state to choose dealer.
isValidForChooseDealer :: PokerGame -> Validation String ()
isValidForChooseDealer FiveCardDraw {..}
  | _gameState /= ChooseDealer = Failure "Game: Wrong game state to set dealer"
  | otherwise                  = Success ()


-- | Function to update the playerTurnIndex of a game.
-- Returns a game with updated playerTurnIndex
setNextPlayerTurn :: PokerGame -> PokerGame
setNextPlayerTurn game =
  game & gamePlayerTurnIndex %~
  fmap (\n -> (n + 1)`mod` length (game ^. gamePlayers))


-- | Function for posting blinds.
-- Returns a game with updated gameState,  gamePlayers data, and gameBets.
postBlinds :: PokerGame -> Either String PokerGame
postBlinds game = do
    toEither . isValidForPostingBlings $ game
    pti <- maybeToEither "Player turn not set" $ game ^. gamePlayerTurnIndex
    let (fst_i, snd_i) = (pti, pti + 1)
    let blind_val = game ^. gameMinBet
    let fst_blind = (game ^. gamePlayers) !! fst_i
    let snd_blind = (game ^. gamePlayers) !! snd_i
    new_fst <- playerBettingAction (Bet blind_val) fst_blind
    new_snd <- playerBettingAction (Bet blind_val) snd_blind
    return $ game & gamePlayers . ix fst_i .~ new_fst
                  & gameBets %~ ((new_fst ^. playerId, Bet blind_val) :)
                  & setNextPlayerTurn
                  & gamePlayers . ix snd_i .~ new_snd
                  & gameBets %~ ((new_snd ^. playerId, Bet blind_val) :)
                  & setNextPlayerTurn
                  & gameState .~ DealingHands

isValidForPostingBlings :: PokerGame -> Validation String ()
isValidForPostingBlings FiveCardDraw {..}
  | _gameState /= PostingBlinds =
    Failure "Game: Wrong game state for posting blinds"
  | otherwise = Success ()


-- | Function to deal hands to players. Returns a game with updated state, deck and players' data.
dealHands :: PokerGame -> Either String PokerGame
dealHands game@FiveCardDraw {_gameState, _gameDeck, _gamePlayers}
  | _gameState /= DealingHands =
    Left "Game: Invalid game state for dealing hands"
  | otherwise = do
    let no_of_cards_to_deal = length _gamePlayers * 5
    (cards_to_deal, newDeck) <- drawCards no_of_cards_to_deal _gameDeck
    let deal = groupSort (zip (cycle _gamePlayers) cards_to_deal) -- deal in round robin fashion
    updatedPlayers <- traverse (uncurry dealHandToPlayer) deal
    return $
      game
        { _gameState = FirstBetRound
        , _gameDeck = newDeck
        , _gamePlayers = updatedPlayers
        }
  where
    dealHandToPlayer :: PokerPlayer -> [Card] -> Either String PokerPlayer
    dealHandToPlayer player cards = do
      h <- mkHand cards
      return $ player {_playerHand = Just h}


-- | Function to apply a betting action to a game.
-- Returns the game with updated players data, gameBets, maybe gameMuck and gameMinBet
inGameBettingAction ::
     Int -> PokerPlayerAction -> PokerGame -> Either String PokerGame
inGameBettingAction player_id action game@FiveCardDraw {..} = do
  toEither $ isValidForBetting game
  toEither $ isValidPlayerTurn player_id game
  let player = (game ^. gamePlayers) !! player_id
  updated_player <- playerBettingAction action player
  let updated_game  =game & gamePlayers . ix player_id .~ updated_player
                          & gameBets %~ ((player_id, action) :)
                          & setNextPlayerTurn
  case action of
        Bet _ -> Left "Invalid action"
        FoldHand -> do
          let folded_hand = maybeToEither "Invalid hand" $ updated_player ^. playerHand
          let folded_player = updated_player & playerHand .~ Nothing
          folded_cards <-  handCards <$> folded_hand
          return $ updated_game & gamePlayers . ix player_id .~ folded_player
                        & gameMuck %~ (folded_cards ++)
        Call n -> return updated_game

        Raise n -> return $ updated_game & gameMinBet .~  n
        AllIn n -> return game



isValidForBetting :: PokerGame -> Validation String ()
isValidForBetting FiveCardDraw {..}
  | _gameState `notElem` [FirstBetRound, SecondBetRound] =
    Failure "> Game: Wrong game state"
  | isNothing _gamePlayerTurnIndex = Failure "> Game: Player Turn not set"
  | otherwise                      = Success ()

isValidPlayerTurn :: Int -> PokerGame -> Validation String ()
isValidPlayerTurn player_id game
  | game ^. gamePlayerTurnIndex /= Just player_id =
    Failure "Game: Invalid player "
  | otherwise = Success ()



playerBettingAction ::
     PokerPlayerAction -> PokerPlayer -> Either String PokerPlayer
playerBettingAction action player =
  getChipsFromPlayer (actionVal action) $ player & playerBettingActions %~
  (action :)
  where
    getChipsFromPlayer :: Int -> PokerPlayer -> Either String PokerPlayer
    getChipsFromPlayer val p
      | player ^. playerChips < val = Left "Player: Insufficient chips"
      | otherwise          = return $ p & playerChips %~ subtract val


-- | Function to discard and draw for a player. Returns a game with updated state, deck and the player data.
discardAndDraw :: Int -> [Int] -> PokerGame -> Either String PokerGame
discardAndDraw pid to_discard game@FiveCardDraw { _gameState
                                                , _gamePlayers
                                                , _gamePlayerTurnIndex
                                                , _gameDeck
                                                , _gameMuck
                                                }
  | _gameState /= Drawing                 = Left "Game: Invalid state for drawing"
  | length to_discard > 5                 = Left "Game: Invalid no of cards to discard"
  | not (isPlayerInGame pid _gamePlayers) = Left "Game: Invalid player id"
  | _gamePlayerTurnIndex /= Just pid      = Left "Game: Invalid player turn"
  | otherwise                             = do
    let current_player = _gamePlayers !! pid
    case _playerHand current_player of
      Nothing -> Left "Empty hand"
      Just current_hand -> do
        (drawn, new_deck) <- drawCards (length to_discard) _gameDeck
        (discarded, new_hand) <- updateHand drawn to_discard current_hand
        let new_player = current_player {_playerHand = Just new_hand}
        let new_players = updateList _gamePlayers pid new_player
        let updated_game =
              game
                { _gameMuck = discarded ++ _gameMuck
                , _gameDeck = new_deck
                , _gamePlayers = new_players
                }
        return updated_game
  where
    isPlayerInGame :: Int -> [PokerPlayer] -> Bool
    isPlayerInGame i pls = i `elem` (_playerId <$> pls)


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
updateList xs i newVal = take i xs ++ [newVal] ++ drop (i + 1) xs

actionVal :: PokerPlayerAction -> Int
actionVal action =
  case action of
    Bet n    -> n
    FoldHand -> 0
    Call n   -> n
    Raise n  -> n
    AllIn n  -> n
