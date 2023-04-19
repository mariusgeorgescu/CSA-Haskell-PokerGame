

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


