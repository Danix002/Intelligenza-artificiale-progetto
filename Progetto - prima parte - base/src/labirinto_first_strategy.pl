% Definizione del labirinto
pos(wall, 0, 0).
pos(empty, 0, 1).
pos(empty, 0, 2).
pos(empty, 0, 3).
pos(empty, 0, 4).
pos(empty, 0, 5).
pos(empty, 0, 6).
pos(empty, 0, 7).

pos(empty, 1, 0).
pos(empty, 1, 1).
pos(empty, 1, 2).
pos(empty, 1, 3).
pos(wall, 1, 4).
pos(empty, 1, 5).
pos(empty, 1, 6).
pos(empty, 1, 7).

pos(empty, 2, 0).
pos(empty, 2, 1).
pos(empty, 2, 2).
pos(empty, 2, 3).
pos(empty, 2, 4).
pos(empty, 2, 5).
pos(empty, 2, 6).
pos(empty, 2, 7).

pos(empty, 3, 0).
pos(empty, 3, 1).
pos(empty, 3, 2).
pos(empty, 3, 3).
pos(empty, 3, 4).
pos(empty, 3, 5).
pos(empty, 3, 6).
pos(empty, 3, 7).

pos(empty, 4, 0).
pos(empty, 4, 1).
pos(empty, 4, 2).
pos(wall, 4, 3).
pos(empty, 4, 4).
pos(empty, 4, 5).
pos(empty, 4, 6).
pos(empty, 4, 7).

pos(empty, 5, 0).
pos(empty, 5, 1).
pos(empty, 5, 2).
pos(empty, 5, 3).
pos(wall, 5, 4).
pos(wall, 5, 5).
pos(empty, 5, 6).
pos(wall, 5, 7).

pos(empty, 6, 0).
pos(empty, 6, 1).
pos(empty, 6, 2).
pos(empty, 6, 3).
pos(wall, 6, 4).
pos(empty, 6, 5).
pos(empty, 6, 6).
pos(monster_position, 6, 7).

pos(wall, 7, 0).
pos(empty, 7, 1).
pos(empty, 7, 2).
pos(portal, 7, 3).
pos(wall, 7, 4).
pos(wall, 7, 5).
pos(empty, 7, 6).
pos(empty, 7, 7).

azione(nord).
azione(sud).
azione(est).
azione(ovest).

size(7, 7).

applicable(nord, pos(monster_position, R, C)) :-
    R > 0,
    R1 is R - 1,
    \+ pos(wall, R1, C).


applicable(est, pos(monster_position, R, C)) :-
    size( Row, Col ),
    C < Col,
    C1 is C + 1,
    \+ pos(wall, R, C1).

applicable(sud, pos(monster_position, R, C)) :-
    size( Row, Col ),
    R < Row,
    R1 is R + 1,
    \+ pos(wall, R1, C).


applicable(ovest, pos(monster_position, R,C)) :-
    C > 0,
    C1 is C - 1,
    \+ pos(wall, R, C1).

ricerca_iterative_deepening(Cammino, FinalVisited):-
    pos(monster_position, R, C),
    iterative_deepening_search(1, [pos(monster_position, R, C)], pos(monster_position, R, C), Cammino, FinalVisited),
    write('position visited by monster: '), print(FinalVisited), nl,
    write('walk: '), print(Cammino), nl.

iterative_deepening_search(Limit, Visited, pos(monster_position, R, C), Cammino, FinalVisited):-
    profondity_search(Limit, pos(monster_position, R, C), Cammino, Visited,  FinalVisited).

iterative_deepening_search(Limit, Visited, pos(monster_position, R, C), Cammino, FinalVisited):-
    \+ profondity_search(Limit, pos(monster_position, R, C), Cammino, Visited, FinalVisited),
    NewLimit is Limit + 1,
    %print('Limit: '), print(Limit), nl,
    iterative_deepening_search(NewLimit, Visited, pos(monster_position, R, C), Cammino,  FinalVisited).

profondity_search(_, pos(monster_position, MonsterRow, MonsterCol), [], Visited, FinalVisited) :- pos(portal, MonsterRow, MonsterCol), FinalVisited = Visited, !.

profondity_search(Limit, pos(monster_position, MonsterRow, MonsterCol), [Az|SeqAzioni], Visited, FinalVisited) :-
    Limit > 0,
    applicable(
        Az, 
        pos(monster_position, MonsterRow, MonsterCol)
    ),
    transform(Az, pos(monster_position, MonsterRow, MonsterCol), Result),
    check_visited(Az, Result, Visited),
    NewLimit is Limit - 1,
    profondity_search(NewLimit, Result, SeqAzioni, [Result | Visited], FinalVisited).

check_visited(_, pos(monster_position, R, C), Visited) :- 
    \+ member(pos(monster_position, R, C), Visited).
    
transform(nord, pos(T, R, C), Result) :- 
    R1 is R - 1,
    Result = pos(T, R1, C), !.

transform(sud, pos(T, R, C), Result) :- 
    R1 is R + 1,
    Result = pos(T, R1, C), !.

transform(est, pos(T, R, C), Result) :- 
    C1 is C + 1,
    Result = pos(T, R, C1), !.

transform(ovest, pos(T, R, C), Result) :- 
    C1 is C - 1,
    Result = pos(T, R, C1), !.