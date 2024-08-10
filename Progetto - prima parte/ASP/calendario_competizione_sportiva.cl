% Definizione delle squadre
squadra(juventus, torino).
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



% Regola per generare le partite (scelta)
{ partita(Squadra1, Squadra2, Giornata) } :-
    squadra(Squadra1, _),
    squadra(Squadra2, _),
    Squadra1 != Squadra2,
    giornata(Giornata).

% Vincolo sul numero di partite per giornata
% Assicurati che ci siano almeno 8 partite per ogni giornata e massimo 8
%8 { partita(Squadra1, Squadra2, Giornata) : squadra(Squadra1, _), squadra(Squadra2, _), Squadra1 != Squadra2 } 8 :- giornata(Giornata).

1 { partita(Squadra1, Squadra2, Giornata) :  squadra(Squadra2, _), Squadra1 != Squadra2 } 1 :- giornata(Giornata), squadra(Squadra1, _).
1 { partita(Squadra1, Squadra2, Giornata) :  squadra(Squadra1, _), Squadra1 != Squadra2 } 1 :- giornata(Giornata), squadra(_, Squadra2).

% 1 { partita(Squadra1, _, Giornata); partita(_, Squadra1, Giornata)} 1 :- giornata(Giornata), squadra(Squadra1, _).





% Definizione delle giornate
giornata(N) :- N = 1.


%:- partita(Squadra, _, Giornata), partita(_, Squadra, Giornata).


% Mostra le partite generate
#show partita/3.