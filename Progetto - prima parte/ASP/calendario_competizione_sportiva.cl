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

giornata(1..30).


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
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), Giornata < 15, Giornata2 < 15.
:- partita(Giornata, SquadraCasa, SquadraTrasferta), partita(Giornata2, SquadraTrasferta, SquadraCasa), Giornata > 14, Giornata2 > 14.






% Mostra le partite generate
#show partita/3.