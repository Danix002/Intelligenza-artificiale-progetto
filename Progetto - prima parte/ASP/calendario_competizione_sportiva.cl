% Definizione delle squadre
squadra(juventus, torinoAllianz).
squadra(milan, milano).
squadra(inter, milano).
squadra(roma, roma).
squadra(salernitana, salerno).
squadra(torino, torino).
squadra(bologna, bologna).
squadra(udinese, udine).
squadra(napoli, napoli).
squadra(genoa, genova).
squadra(fiorentina, firenze).
squadra(atalanta, bergamo).
squadra(cagliari, cagliari).
squadra(verona, verona).
squadra(empoli, empoli).
squadra(parma, parma).


% partita(1,napoli,juventus).
% partita(2,fiorentina,juventus).
% partita(3,juventus,cagliari).
% partita(4,roma,juventus).
% partita(5,juventus,empoli).
% partita(6,inter,juventus).
% partita(7,torino,juventus).
% partita(8,juventus,milan).
% partita(9,atalanta,juventus).
% partita(10,juventus,parma).
% partita(11,salernitana,juventus).
% partita(12,juventus,verona).
% partita(13,genoa,juventus).
% partita(14,bologna,juventus).
% partita(15,juventus,udinese).
% partita(16,cagliari,juventus).
% partita(17,udinese,juventus).
% partita(18,juventus,genoa).
% partita(19,verona,juventus).
% partita(20,juventus,fiorentina).
% partita(21,juventus,atalanta).
% partita(22,parma,juventus).
% partita(23,juventus,inter).
% partita(24,juventus,salernitana).
% partita(25,empoli,juventus).
% partita(26,juventus,torino).
% partita(27,juventus,bologna).
% partita(28,milan,juventus).
% partita(29,juventus,roma).
% partita(30,juventus,napoli).


% Definizione delle giornate
giornata(1..15, andata).
giornata(16..30, ritorno).

% Generatore di partite: seleziona esattamente 8 partite tra tutte le coppie di squadre diverse per ogni giornata
1 {
    partita(Giornata, Casa, Trasferta) : 
    giornata(Giornata, _)
} 1 :- match(Casa, Trasferta).

match(Casa, Trasferta) :- squadra(Casa,_), squadra(Trasferta, _), Casa != Trasferta.



% Vincolo: una squadra può partecipare a una sola partita per giornata
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata, SquadraCasa, SquadraTrasferta2), SquadraTrasferta != SquadraTrasferta2.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata, SquadraCasa2, SquadraTrasferta), SquadraCasa != SquadraCasa2.

% Vincolo: la stessa squadre nella stessa giornata non possono fare la partita di andata e ritorno 
:- partita(Giornata, SquadraCasa, _), partita(Giornata, _, SquadraCasa).

% Vincolo: la stessa partita può avvenire solo in una giornata
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraCasa, SquadraTrasferta), Giornata != Giornata2.

% Vincolo: divisione delle partite di andata (prime 15) e del ritorno (ultime 15)
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), giornata(Giornata, andata), giornata(Giornata2, andata).
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), giornata(Giornata, ritorno), giornata(Giornata2, ritorno).

%Vincolo: la stessa squadra non deve giocare più di due partite consecutive in casa o in trasferta 
:- partita(Giornata, SquadraCasa, _), partita(Giornata1, SquadraCasa, _), partita(Giornata2, SquadraCasa, _), Giornata1 = Giornata+1, Giornata2 = Giornata1+1.
:- partita(Giornata, _, SquadraTrasferta), partita(Giornata1, _, SquadraTrasferta), partita(Giornata2, _, SquadraTrasferta), Giornata1 = Giornata+1, Giornata2 = Giornata1+1.

% Vincolo: le squadre con lo stesso stadio non devono giocare in casa contemporaneamente nella stessa giornata
:- partita(Giornata, SquadraCasa, _), squadra(SquadraCasa, Stadio), partita(Giornata, SquadraCasa1, _), squadra(SquadraCasa1, Stadio1), SquadraCasa != SquadraCasa1, Stadio = Stadio1.


% Mostra le partite generate
#show partita/3.
