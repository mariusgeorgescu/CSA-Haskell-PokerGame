

PlayerLobbyAction
  - Join name pk sig seedhash
  - Reveal pk sig seed 

PlayerBettingAction  : SmallBlindBet Check/Call Raise Fold AllIn

PlayerDrawingAction : NoDraw Draw 



--- execAction pa -> m ()



O actiune care schimba starea jocului:
  - trebuie semnata de jucatorul al carui rand este sa actioneze    -- isValidAction pk sig action -> m bool
  - trebuie sa fie potrivita pentru momentul jocului --- isValidAction pa  -> m bool
  - daca are parametrii, acestia trebuie sa fie valizi in functie de starea jocului  --- isValidAction pa  -> m bool



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


