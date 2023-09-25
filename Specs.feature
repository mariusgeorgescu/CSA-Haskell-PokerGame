
P1 - hash permutare 1-52 + hash permutare 5-52
P2 - hash permutare 1-52 + hash 5 indexi 5-52

P2 - reveal 1-51  -- P1 stie cartile lui
P1 - reveal 



P2 -  permutare + (5 indecsi pt P1) si (hash indecsi P2)  -- P1 stie mana lui
P1 

P1 - Reveal hash deck   --- P2 stie mana lui

P2 - Reveal
pachet permutat






Alice , shuffle and encrypt deck  ---- shuffled deck encrypted by alice

Bob 
choose hands (5 indeces for him) (5 indeces for alice) 
bob posts 5 indeces for alice and 5 encrypted bs 
--- Alice Hand encrypted by alice
--- Bob Hand encrypted by alice and bob

Alice decripts bob hand and post 5 bs
-- BobHand -- encrypted by bob
-- AliceHand encrypted by Alice


Alice post key
  -- decrypt deck and check if valid   -- validam ca alice a pus pachet corect
  -- decrypt bobhandencbybobandalice and see if match bobhandencrypted by bob -- validam ca alice a fost corecta cand a dat val decriptate

Bob post key
  -- decrypt bobhandencbybobandalice   --- and check if valid hand (cards in deck encrypted by alice exept alice hand)
      -- validam ca bob a fost corect cand si-a ales mana


--- Validate 
    -- deck valid
    -- hands valid (no duplicates)

Contract {
  GameParams
  GameState
}
validator :: GameParams -> GameState -> Contract -> Bool


-- DATUM
GameState {
  e1_Deck :: [ByteString]
  e1_P1Hand :: Maybe [ByteString] 
  e1_e2_P2Hand :: Maybe [ByteString] 
  e2_P2Hand :: Maybe [ByteString]  
  priv_P1 :: Maybe [PrivateKey]
  priv_P2 :: Maybe [PrivateKey]
}
-- REDEMER
P2_ChooseHands :: ([ByteString], [ByteString])
  - set e1_P1Hand and  e1_e2_P2Hand
P1_UnlockP2Hand  [ByteString]
  - set e2_P2Hand
P1_Reveal (PrivateKey)
P2_Reveal (PrivateKey)


PlayerLobbyAction
  - Join name pk sig seedhash
  - Reveal pk sig seed 

PlayerBettingAction  : SmallBlindBet Check/Call Raise Fold AllIn

PlayerDrawingAction : NoDraw Draw 



--- execAction pa -> m ()

Jocul se joaca in 2 jucatori;
# deck, sgp1deck
Primul jucator initiaza contractul si stabilieste 
  -min bet
  -table chips
  - pachet in clar si encryptat SgP1

#SGP1SGP2 (sgp1sgp2 deck)
Cel de al doilea jucator encrypteza pachetul cu cheie unica si amesteca (SgP1-SgP2)

#Mkey (deck)
P1 decripteaza encrypteaza multikey (SgP2-MkP1)

P2 decripteaza si encrypteaza mkey (MkP1 - MkP2)


P1 da chei (mp1v[i])

P2  verifica :
   decrypt mp1v[i] (decrypt sig2 (SgP2-MkP1)) == decrypt mp1v[i] (decrypt mp2v[i] (MkP1 - MkP2)  -- show

  encrypt mp2v[i] (decrypt sig2 SgP2-MkP1)) ==  (MkP1 - MkP2) [i] 
        --proof valid mkey and sig2

P2 da chei
P1 verifica 
  encrypt mp2v[1] mp1v[i] i = (MkP1 - MkP2)

  encrypt mv1 (decrypt sig1 SgP1-SgP2)) ==  SgP2-MkP1) [i] 
        --proof valid mv1 and sig1



O actiune care schimba starea jocului:
  - trebuie semnata de jucatorul al carui rand este sa actioneze    -- isValidAction pk sig action -> m bool
  - trebuie sa fie potrivita pentru momentul jocului --- isValidAction pa  -> m bool
  - daca are parametrii, acestia trebuie sa fie valizi in functie de starea jocului  --- isValidAction pa  -> m bool



Tipuri de actiuni

  - JoinAction (secrethash, sgKeyEncryptedDeck) -- hash secret pentru randomness 
  - RevealAction (secret, multiKeyEncryptedDeck)  -- secret
  - UnlockCardsAction [secrets]
  - BettingAction  : SmallBlindBet | Call | Raise Int | Fold | AllIn
  - DrawingAction : NoDraw | Draw [Int] 
  - ShowdownAction : ([hashcards]) 


Inante de inceperea jocului:
  Jucatori intra in lobby cu:
    - identitate
    - hash de secret pentru sursa de randomness

Cand numarul de jucatori e suficient jocul poate sa inceapa
  -jucatorii trebuie sa poata sa dovedeasca secretul

Dupa ce toti jucatorii au aratat secretul
  - dealerul se seteaza si ordinea jucatorilor
  - acesta incepe amestecarea pachetului (encrypt all and shuffle ) round + encrypt indiv round

  - post blinds 


