#   = NotStarted -- ^ The initial state of the game, where players are not yet playing.
#   | ChooseDealer -- ^ Choose who is the dealer and determine the blinds based on that.
#   | PostingBlinds -- ^ Before each hand, the two players to the left of the dealer are required to post blind bets, which are small forced bets that begin the pot.
#   | DealingHands -- ^ After the blinds have been posted, the dealer deals five cards to each player, face down
#   | FirstBetRound -- ^ Once the cards have been dealt, the first round of betting begins. Players can either fold, call (match the previous bet), or raise (increase the previous bet).
#   | Drawing -- ^ After the first round of betting, players have the option to discard some or all of their cards and receive new ones from the dealer.
#   | SecondBetRound -- ^ After the drawing round, another round of betting begins, with the same options as before: fold, call, or raise
#   | Showdown -- ^ If more than one player is still in the hand after the second round of betting, a showdown occurs, where the players reveal their cards and the best hand wins the pot.
#   | EndOfHand -- ^ After the pot has been awarded, the next hand begins, with the player to the left of the previous dealer becoming the new dealer.

# -- | Game
# data PokerGame =
#   FiveCardDraw
#     { gameState           :: GameState
#     , gameDeck            :: Deck
#     , gameMuck            :: [Card]
#     , gameBets            :: [(Int, PokerPlayerAction)]
#     , gameDealerIndex     :: Maybe Int
#     , gamePlayerTurnIndex :: Maybe Int
#     , gameMinBet          :: Int
#     , gamePlayers         :: [PokerPlayer]
#     }


# -- | Player
# data PokerPlayer =
#   PokerPlayer
#     { 
#     , playerHand           :: Maybe Hand
#     , playerId             :: Int
#     , playerName           :: String
#     , playerChips          :: Int
#     }





@isValidNotStarted 
      @gameState == @NotStarted
      @gameDeck isValidDeck
      @gameMuck isEmpty
      @gameBets isEmpty
      @gameDealerIndex == Nothing
      @gamePlayerTurnIndex == Nothing
      @gamePlayers length less then or equal to 5


@NotStarted
Feature: Poker Game Lifecycle
  Background: PokerGame initiated in @NotStarted state
    Given @isValidNotStarted  

      
  Scenario: Add new player to game 
    Given Valid PokerGame in @NotStarted state
      When New player joins the game
        Given @isValidPlayer
        Then 
          If length of @gamePlayers less than minPlayersToStart
            Return new game such that
              @gamePlayers == @Player : @gamePlayers  -- Deep equality
              @isValidNotStarted
              rest of are values equal 
          else length of @gamePlayers + 1 greater than minPlayersToStart
            add player to game
            set dealer
            deal hands 
            place min bets
            Return new game such that 
              @gameState == @FirstBetRound
              @gameDeck == corect player has correct cards
              @gameMuck is empty
              @gameBets == corect players have minbets are placed
              @gameDealerIndex == setDealerIndex        
              @gamePlayerTurnIndex == (setDealerIndex + 1) mod length @gamePlayers       
              @gamePlayers == correct players have bets placed

  Scenario: Start poker game
    Given Valid PokerGame in @NotStarted state
      When Game started 
        Given 

  Scenario: Place bet
    Given Valid PokerGame in @FirstBetRound state
     When Payer places bet
      Given 
        @gamePlayerTurnIndex is player's id 
        players bet >= @valueToCall
        players chips - bet >= 0



isBettingRoundOver 
  - noNewRaises -- check last action of each player
  - allPlayersActed -- length of action of all players must be equal
  
if isOver and all folded ->endGame game  
elseDrawing  