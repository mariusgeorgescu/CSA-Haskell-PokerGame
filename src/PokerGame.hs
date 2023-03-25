{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module PokerGame where

import           Cards              (Card, Deck, drawCards, mkFullDeck,
                                     shuffleDeck)

import           Data.Bifunctor     (Bifunctor (bimap))
import           Data.Either.Extra  (maybeToEither)
import           Data.IntMap        (insert, keys, lookup, size, toList, update)
import           Data.IntMap.Strict (IntMap, elems, fromList, insertWith)
import           Data.List          (nub)
import           Data.List.Extra    (allSame, groupSort, intercalate, partition)
import           Data.Maybe         (fromMaybe)
import           Data.Validation    (Validation (..), toEither)
import           GHC.Unicode        (isAlpha)
import           Prelude            hiding (lookup)
import           Test.QuickCheck    (Arbitrary (arbitrary), Gen, vectorOf)

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------
-- | Player actions
data PokerPlayerAction
  = SmallBlind Int
  | FoldHand
  | Call Int
  | Raise Int
  | AllIn Int
  deriving (Show)


-- | Game Params
data GameSettings =
  GameSettings
    { settingsMinBet            :: Int -- ^ Minimum bet value
    , settingsMinPlayersToStart :: Int -- ^ Minimum no of players to start the game
    }
  deriving (Show)


-- | Game state
data GameState
  = NotStarted -- ^ The initial state of the game, where players are not yet playing.
  | DealerSet -- ^ Choose who is the dealer and determine the blinds based on that.
  | PostingBlinds -- ^ Before each hand, the two players to the left of the dealer are required to post blind bets, which are small forced bets that begin the pot.
  | HandsDealt -- ^ After the blinds have been posted, the dealer deals five cards to each player, face down
  | FirstBetRound -- ^ Once the cards have been dealt, the first round of betting begins. Players can either fold, call (match the previous bet), or raise (increase the previous bet).
  | Drawing -- ^ After the first round of betting, players have the option to discard some or all of their cards and receive new ones from the dealer.
  | SecondBetRound -- ^ After the drawing round, another round of betting begins, with the same options as before: fold, call, or raise
  | Showdown -- ^ If more than one player is still in the hand after the second round of betting, a showdown occurs, where the players reveal their cards and the best hand wins the pot.
  | EndOfHand -- ^ After the pot has been awarded, the next hand begins, with the player to the left of the previous dealer becoming the new dealer.
  deriving (Eq, Show)


-- | Game
data PokerGame =
  FiveDraw
    { gameState           :: GameState
    , gameDeck            :: Deck
    , gameSettings        :: GameSettings
    , gamePlayers         :: IntMap PokerPlayer
    , gameMuck            :: [Card]
    , gameDealerIndex     :: Maybe Int
    , gamePlayerTurnIndex :: Maybe Int
    , gamePlayersNonce    :: IntMap Int
    , gamePlayersChips    :: IntMap Int
    , gamePlayersBets     :: IntMap [PokerPlayerAction]
    , gameMaxBet          :: Maybe Int
    }


-- | Player
data PokerPlayer =
  PokerPlayer
    { playerId   :: Int
    , playerName :: String
    , playerHand :: Maybe Hand
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
  show = showPlayer

instance Show PokerGame where
  show :: PokerGame -> String
  show = showGame

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
mkPokerPlayer :: Int -> String -> Either String PokerPlayer
mkPokerPlayer = (toEither .) . mkPokerPlayer'

mkPokerPlayer' :: Int -> String -> Validation String PokerPlayer
mkPokerPlayer' p_id p_name =
  PokerPlayer <$> valid_id <*> valid_name <*> Success Nothing
  where
    valid_id =
      if p_id < 0
        then Failure "| Player: Invalid ID"
        else Success p_id
    valid_name =
      if null p_name || (not . all isAlpha) p_name
        then Failure $ "| Player: Invalid name: " ++ p_name
        else Success p_name

-------------------------------------------------------------------------------
-- * Game functions
-------------------------------------------------------------------------------

-- | Function to initate a PokerGame
initPokerGame :: [Int] -> Int -> Int -> Either String PokerGame
initPokerGame perm minBet psToStart = do
  deck <- shuffleDeck perm mkFullDeck
  gs <- toEither mkGameSettings
  Right $
    FiveDraw
      NotStarted
      deck
      gs
      mempty
      mempty
      Nothing
      Nothing
      mempty
      mempty
      mempty
      Nothing
  where
    mkGameSettings :: Validation String GameSettings
    mkGameSettings
      | minBet < 1 = Failure "GameSettings: invalid minimum bet"
      | (||) <$> (< 2) <*> (> 5) $ psToStart =
        Failure "GameSettings: invalid no of players"
      | otherwise = Success $ GameSettings minBet psToStart


-- | Function to add a player to a game.
addPlayerToGame :: String -> Int -> Int -> PokerGame -> Either String PokerGame
addPlayerToGame p_name p_chips p_nonce game@FiveDraw {..}
  | gameState /= NotStarted               = Left "Game: Invalid state for adding new player"
  | p_chips < settingsMinBet gameSettings = Left "Game: Invalid chips value"
  | otherwise =
    let p_id = size gamePlayers
     in if (p_id + 1) > 5
          then Left "Game: To many players"
          else do
            player <- mkPokerPlayer p_id p_name
            if p_id + 1 < settingsMinPlayersToStart gameSettings
              then return
                     game
                       { gamePlayers = insert p_id player gamePlayers
                       , gamePlayersNonce = insert p_id p_nonce gamePlayersNonce
                       , gamePlayersChips = insert p_id p_chips gamePlayersChips
                       }
              else do
                let gameWithPlayer =
                      game
                        { gamePlayers = insert p_id player gamePlayers
                        , gamePlayersNonce =
                            insert p_id p_nonce gamePlayersNonce
                        , gamePlayersChips =
                            insert p_id p_chips gamePlayersChips
                        }
                setDealer gameWithPlayer >>= postBlinds >>= dealHands

determineDealer :: PokerGame -> Either String Int
determineDealer FiveDraw {..}
  | gameState /= NotStarted = Left "Game: Invalid state to set dealer"
  | otherwise               = Right $ sum gamePlayersNonce `mod` size gamePlayersNonce


-- | Function to set the dealer of a poker game.
setDealer :: PokerGame -> Either String PokerGame
setDealer game@FiveDraw {..}
  | gameState /= NotStarted = Left "Game: Invalid state to set dealer"
  | otherwise               = do
    dealer_id <- determineDealer game
    return $
      game
        { gameState = DealerSet
        , gameDealerIndex = Just dealer_id
        , gamePlayerTurnIndex = Just $ (dealer_id + 1) `mod` size gamePlayers
        , gamePlayersBets = mempty
        }


-- | Function for posting blinds.
postBlinds :: PokerGame -> Either String PokerGame
postBlinds game@FiveDraw {..}
  | gameState /= DealerSet =
    Left $ "Game: Invalid state for posting blinds: " ++ show gameState
  | otherwise =
    let min_bet = settingsMinBet gameSettings
     in playerBettingAction (SmallBlind min_bet) =<<
        playerBettingAction
          (SmallBlind min_bet)
          game {gameState = PostingBlinds, gameMaxBet = Just min_bet}


-- | Function to deal hands to players.
dealHands :: PokerGame -> Either String PokerGame
dealHands game@FiveDraw {..}
  | gameState /= PostingBlinds = Left "Game: Wrong state to deal hands"
  | otherwise                  = do
    let no_of_cards_to_deal = length gamePlayers * 5
    (cards_to_deal, newDeck) <- drawCards no_of_cards_to_deal gameDeck
    let deal = groupSort (zip (cycle (keys gamePlayers)) cards_to_deal) -- deal in round robin fashion
    updatedPlayers <- traverse (dealHandToPlayer game) deal
    return $
      game
        { gameDeck = newDeck
        , gamePlayers = fromList updatedPlayers
        , gameState = FirstBetRound
        }
  where
    dealHandToPlayer ::
         PokerGame -> (Int, [Card]) -> Either String (Int, PokerPlayer)
    dealHandToPlayer FiveDraw {gamePlayers = gp} (k, cards) = do
      player <- maybeToEither "error" $ lookup k gp
      h <- mkHand cards
      return (k, player {playerHand = Just h})

playerBettingAction :: PokerPlayerAction -> PokerGame -> Either String PokerGame
playerBettingAction action game@FiveDraw {..}
  | gameState `notElem` [FirstBetRound, SecondBetRound, PostingBlinds] =
    Left $ "Game: Invalid State for Betting Action" ++ show gameState
  | otherwise = do
    pti <- getCurrentPlayerId game
    p_chips <- getCurrentPlayerChips game
    if actionVal action > p_chips
      then Left $ "Invalid action : Insufficient chips.\nAvailable chips:" ++ show p_chips ++ " and trying to " ++ show action
      else return
             game
               { gamePlayersChips =
                   insertWith subtract pti (actionVal action) gamePlayersChips
               , gamePlayersBets = insertWith (++) pti [action] gamePlayersBets
               , gamePlayerTurnIndex = Just $ (pti + 1) `mod` size gamePlayers
               }


-- | Function to apply a betting action to a game.
-- Returns the game with updated players data, gameBets, maybe gameMuck and gameMinBet
roundBettingAction :: PokerPlayerAction -> PokerGame -> Either String PokerGame
roundBettingAction action game@FiveDraw {..} = do
  pti <- getCurrentPlayerId game
  current_player <- getCurrentPlayer game
  let current_actions = getCurrentPlayerBets game
  let current_player_bet = sum $ actionVal <$> current_actions
  chips_bets_turn <- playerBettingAction action game
  updated_game <-
    case action of
      SmallBlind _ -> Left $ "Invalid action: " ++ show action
      FoldHand -> do
        folded_hand <- getCurrentPlayerHand game
        let folded_player = current_player {playerHand = Nothing}
        let folded_cards = handCards folded_hand
        return $
          chips_bets_turn
            { gamePlayers = update (Just . const folded_player) pti gamePlayers
            , gameMuck = folded_cards ++ gameMuck
            }
      Call n ->
        if Just (n + current_player_bet) /= gameMaxBet
          then Left "Invalid call action"
          else return chips_bets_turn
      Raise n ->
        if gameMaxBet >= Just (n + current_player_bet)
          then Left "Invalid raise action"
          else return
                 chips_bets_turn {gameMaxBet = Just (n + current_player_bet)}
      AllIn _ -> return game
  checkIfOver updated_game

checkIfOver :: PokerGame -> Either String PokerGame
checkIfOver game@FiveDraw {..} =
  if allPlayersActed && noNewRaises
    then if allButOneFolded
           then return game {gameState = EndOfHand}
           else if allButOneRemainingAllIn
                  then return $ game {gameState = Showdown}
                  else return $ game {gameState = Drawing}
    else return game
  where
    allPlayersActed = allSame $ length <$> elems gamePlayersBets
    noNewRaises     = not (any isRaise (head <$> elems gamePlayersBets))
    remaining       = filter (not . isFold) <$> elems gamePlayersBets
    allButOneFolded = length remaining == 1
    allButOneRemainingAllIn =
      length (filter (not . isAllIn) <$> elems gamePlayersBets) <= 1

isRaise :: PokerPlayerAction -> Bool
isRaise (Raise _) = True
isRaise _         = False

isFold :: PokerPlayerAction -> Bool
isFold FoldHand = True
isFold _        = False

isAllIn :: PokerPlayerAction -> Bool
isAllIn (AllIn _) = True
isAllIn _         = False


-- | Function to discard and draw for a player. Returns a game with updated state, deck and the player data.
discardAndDraw :: [Int] -> PokerGame -> Either String PokerGame
discardAndDraw to_discard game@FiveDraw {..}
  | gameState /= Drawing  = Left "Game: Invalid state for drawing"
  | length to_discard > 5 = Left "Game: Invalid no of cards to discard"
  | otherwise             = do
    pti <- getCurrentPlayerId game
    current_player <- getCurrentPlayer game
    case playerHand current_player of
      Nothing -> Left "Empty hand"
      Just current_hand -> do
        (drawn, new_deck) <- drawCards (length to_discard) gameDeck
        (discarded, new_hand) <- updateHand drawn to_discard current_hand
        let new_player = current_player {playerHand = Just new_hand}
        let new_players = update (Just . const new_player) pti gamePlayers
        let updated_game =
              game
                { gameMuck = discarded ++ gameMuck
                , gameDeck = new_deck
                , gamePlayers = new_players
                }
        return updated_game

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
    SmallBlind n -> n
    FoldHand     -> 0
    Call n       -> n
    Raise n      -> n
    AllIn n      -> n

---
--- GAME UTILITIES
---
getCurrentPlayer :: PokerGame -> Either String PokerPlayer
getCurrentPlayer g@FiveDraw {..} = do
  pti <- getCurrentPlayerId g
  maybeToEither "Error: Invalid player id" $ lookup pti gamePlayers

getCurrentPlayerName :: PokerGame -> Either String String
getCurrentPlayerName game = do
  player <- getCurrentPlayer game
  return $ playerName player

getCurrentPlayerId :: PokerGame -> Either String Int
getCurrentPlayerId FiveDraw {..} =
  maybeToEither "Error: Invalid player id 1" gamePlayerTurnIndex

getCurrentPlayerBets :: PokerGame -> [PokerPlayerAction]
getCurrentPlayerBets FiveDraw {..} = do
  case gamePlayerTurnIndex of
    Nothing  -> []
    Just pti -> fromMaybe [] $ lookup pti gamePlayersBets

getCurrentPlayerChips :: PokerGame -> Either String Int
getCurrentPlayerChips game@FiveDraw {..} = do
  pti <- getCurrentPlayerId game
  maybeToEither "Error: Invalid player id 3" $ lookup pti gamePlayersChips

getCurrentPlayerHand :: PokerGame -> Either String Hand
getCurrentPlayerHand game = do
  player <- getCurrentPlayer game
  maybeToEither "Invalid hand" $ playerHand player

---
--- Show
----
showPlayer :: PokerPlayer -> String
showPlayer PokerPlayer {..} =
  "\tPLAYER #" ++
  show playerId ++
  " " ++
  playerName ++
  "\n" ++ "\tHand : " ++ maybe "empty hand" show playerHand ++ "\n"

showPlayerInGame :: PokerGame -> Int -> String
showPlayerInGame FiveDraw {..} p_id =
  let p_chips = maybe "No bets" show $ lookup p_id gamePlayersChips
      p_bets = maybe "No bets" show $ lookup p_id gamePlayersBets
      player = maybe "No player" show $ lookup p_id gamePlayers
   in player ++ "\tBets : " ++ p_bets ++ "\n\tChips: " ++ p_chips ++ "\n"

showGame :: PokerGame -> String
showGame game@FiveDraw {..} =
  "------------------------------------\n" ++
  "------| POKER GAME DETAILS |------- \n" ++
  "------------------------------------\n" ++
  "Current state : " ++
  show gameState ++
  "\n" ++
  "Game settings: " ++
  show gameSettings ++
  "\n" ++
  "Dealer: " ++
  maybe "Not set" playerName (flip lookup gamePlayers =<< gameDealerIndex) ++
  "\n" ++
  "Player turn: " ++
  maybe "Not set" playerName (flip lookup gamePlayers =<< gamePlayerTurnIndex) ++
  "\n" ++
  "Deck :" ++
  show gameDeck ++
  "\n" ++
  "Muck :" ++
  show gameMuck ++
  "\n" ++
  "Bets :" ++
  show (toList gamePlayersBets) ++
  "\n" ++
  "Chips :" ++
  show (toList gamePlayersChips) ++
  "\n" ++
  "Players :\n" ++
  "------------------------------------\n" ++
  intercalate "\n" (showPlayerInGame game <$> keys gamePlayers) ++
  "\n" ++ "------------------------------------"
