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
8 { partita(Giornata, Squadra1, Squadra2 ) : 
    squadra(Squadra1, _), 
    squadra(Squadra2, _), 
    Squadra1 != Squadra2 
} 8 :- giornata(Giornata).

% Vincolo: Una squadra può partecipare a una sola partita per giornata
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata, SquadraCasa, Squadra3), SquadraTrasferta != Squadra3.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata, Squadra3, SquadraTrasferta), SquadraCasa != Squadra3.
:- partita(Giornata,SquadraCasa, _), partita(Giornata, _, SquadraCasa).
:- partita(Giornata,SquadraCasa,SquadraTrasferta ), partita(Giornata2,SquadraCasa,SquadraTrasferta), Giornata != Giornata2.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), Giornata < 10, Giornata2 < 10.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), Giornata > 9, Giornata2 > 9.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata+1, SquadraCasa, SquadraTrasferta2), partita(Giornata+2, SquadraCasa, SquadraTrasferta3).
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata+1, SquadraCasa2, SquadraTrasferta), partita(Giornata+2, SquadraCasa3, SquadraTrasferta).





% Mostra le partite generate
#show partita/3.