% Definizione dello stato iniziale: posizioni iniziali del mostriciattolo, martello, gemme e muri di ghiaccio.
initial_state(state(pos(1,1), pos(3,3), [pos(2,2), pos(4,4), pos(6,6)], [pos(3,2), pos(5,5)])).

% Definizione dello stato obiettivo: raggiungere il portale
goal_state(state(PosMostriciattolo, _, _, _)) :- portale(PosMostriciattolo).

% Definizione del portale
portale(pos(8,8)).

% Movimento nelle quattro direzioni
move(state(pos(X,Y), Hammer, Gems, IceWalls), state(pos(X2,Y2), Hammer, Gems, IceWalls)) :-
    (X2 is X + 1, Y2 is Y; % sud
     X2 is X - 1, Y2 is Y; % nord
     X2 is X, Y2 is Y + 1; % est
     X2 is X, Y2 is Y - 1), % ovest
    valid_position(pos(X2,Y2), IceWalls).

% Controllo della validità della posizione (non deve essere un muro invalicabile)
valid_position(pos(X,Y), IceWalls) :-
    X >= 1, X =< 8, Y >= 1, Y =< 8, % entro i limiti del labirinto
    \+ member(pos(X,Y), IceWalls). % non è un muro invalicabile

% Implementazione della BFS
bfs(Start, Path) :-
    bfs([[Start]], [], Path).

bfs([[State|Path]|_], _, [State|Path]) :-
    goal_state(State).

bfs([[State|Path]|Rest], Visited, Solution) :-
    findall([Next,State|Path],
            (move(State, Next),
             \+ member(Next, Visited)),
            NewPaths),
    append(Rest, NewPaths, Queue),
    bfs(Queue, [State|Visited], Solution).

% Esecuzione dell'algoritmo BFS con lo stato iniziale
run_bfs :-
    initial_state(Start),
    bfs(Start, Path),
    reverse(Path, Solution),
    print_solution(Solution).

% Stampa della soluzione trovata
print_solution([]).
print_solution([State|Rest]) :-
    write(State), nl,
    print_solution(Rest).

% Avvia l'algoritmo
:- run_bfs.
