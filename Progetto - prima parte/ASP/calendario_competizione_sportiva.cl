% Definizione delle squadre
squadra(juventus, torinoAllianz).
squadra(milan, milano).
squadra(inter, milano).
squadra(roma, roma).
squadra(lazio, roma).
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

giornata(1..23).

% Regola per generare le partite (scelta)
{ partita(Giornata, Squadra1, Squadra2) } :-
    squadra(Squadra1, _),
    squadra(Squadra2, _),
    Squadra1 != Squadra2,
    giornata(Giornata).

% Vincolo sul numero di partite per giornata
% Assicurati che ci siano esattamente 8 partite per ogni giornata
8 { partita(Giornata, Squadra1, Squadra2) : 
    squadra(Squadra1, _), 
    squadra(Squadra2, _), 
    Squadra1 != Squadra2 
} 8 :- giornata(Giornata).

% Vincolo: una squadra può partecipare a una sola partita per giornata
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata, SquadraCasa, SquadraTrasferta2), SquadraTrasferta != SquadraTrasferta2.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata, SquadraCasa2, SquadraTrasferta), SquadraCasa != SquadraCasa2.

% Vincolo: due squadre nella stessa giornata non possono fare la partita di andata e ritorno tra di loro
:- partita(Giornata, SquadraCasa, _), partita(Giornata, _, SquadraCasa).

% Vincolo: la stessa partita può avvenire solo in una giornata
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraCasa, SquadraTrasferta), Giornata != Giornata2.

% Vincolo: divisione delle partite di andata (prime 15) e del ritorno (ultime 15)
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), Giornata < 10, Giornata2 < 10.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), Giornata > 9, Giornata2 > 9.

% Vincolo: la stessa squadra non deve giocare più di due partite consecutive in casa o in trasferta 
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata+1, SquadraCasa, SquadraTrasferta2), partita(Giornata+2, SquadraCasa, SquadraTrasferta3).
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata+1, SquadraCasa2, SquadraTrasferta), partita(Giornata+2, SquadraCasa3, SquadraTrasferta).

% Vincolo: le squadre con lo stesso stadio non devono giocare in casa contemporaneamente nella stessa giornata
:- partita(Giornata, SquadraCasa, _), squadra(SquadraCasa, Stadio), partita(Giornata, SquadraCasa1, _), squadra(SquadraCasa1, Stadio1), SquadraCasa != SquadraCasa1, Stadio = Stadio1.

% Mostra le partite generate
#show partita/3.