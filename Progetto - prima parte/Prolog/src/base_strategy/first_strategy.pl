applicable(nord, pos(monster_position, R, C)) :-
    R > 0,
    R1 is R - 1,
    \+ pos(wall, R1, C).

applicable(est, pos(monster_position, R, C)) :-
    size(_, Col),
    C < Col,
    C1 is C + 1,
    \+ pos(wall, R, C1).

applicable(sud, pos(monster_position, R, C)) :-
    size(Row, _),
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