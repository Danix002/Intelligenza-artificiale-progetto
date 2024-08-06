:-[knowledge_example].
:-[applicable].
:-[transform].
:-[utility].

ricerca_iterative_deepening(Cammino, FinalVisited):-
    pos(monster_position, R, C),
    findall(pos(gem, RG, CG), pos(gem, RG, CG), Lpos),
    has_hammer(HammerTaked),
    iterative_deepening_search(1, [[pos(monster_position, R, C) | Lpos]], pos(monster_position, R, C), Lpos, HammerTaked, _, Cammino, FinalVisited, _),
    %write('position visited by monster: '), print(FinalVisited), nl,
    %write('hammer taked: '), print(HammerTaked1), nl,
    %write('free cells: '), print(FreeCellsFinal), nl,
    write('walk: '), print(Cammino), nl.

iterative_deepening_search(Limit, Visited, pos(monster_position, R, C), Lpos, HammerTaked, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal):-
    profondity_search(Limit, pos(monster_position, R, C), Lpos, Cammino, Visited, FinalVisited, HammerTaked, HammerTaked1, [], FreeCellsFinal).

iterative_deepening_search(Limit, Visited, pos(monster_position, R, C), Lpos, HammerTaked, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal):-
    \+ profondity_search(Limit, pos(monster_position, R, C), Lpos, Cammino, Visited, FinalVisited, HammerTaked, HammerTaked1, [], FreeCellsFinal),
    NewLimit is Limit + 1,
    iterative_deepening_search(NewLimit, Visited, pos(monster_position, R, C), Lpos, HammerTaked, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal).

profondity_search(_, pos(monster_position, MonsterRow, MonsterCol), _, [], Visited, FinalVisited,HammerTaked, HammerTaked1, FreeCells, FreeCellsFinal) :- pos(portal, MonsterRow, MonsterCol), HammerTaked1 is HammerTaked, FreeCellsFinal = FreeCells, FinalVisited = Visited, !.

profondity_search(Limit, pos(monster_position, MonsterRow, MonsterCol), GemState, [Az|SeqAzioni], Visited, FinalVisited, HammerTaked, HammerTaked1, FreeCells, FreeCellsFinal) :-
    Limit > 0,
    applicable(
        Az, 
        pos(monster_position, MonsterRow, MonsterCol),
        [pos(monster_position, MonsterRow, MonsterCol) | GemState],
        HammerTaked,
        FreeCells
    ),
    init_transform(Az, [pos(monster_position, MonsterRow, MonsterCol) | GemState], Visited, [TransformedPositionMonster | TransformedPositionGem], HammerTaked, NewHammerTaked1, FreeCells, NewFreeCells),
    sort_by_column(TransformedPositionGem, SortTransformedPositionGemColumn),
    sort_by_column(SortTransformedPositionGemColumn, SortTransformedPositionGem),
    NewLimit is Limit - 1,
    profondity_search(NewLimit, TransformedPositionMonster, TransformedPositionGem, SeqAzioni, [[TransformedPositionMonster | SortTransformedPositionGem] | Visited],  FinalVisited, NewHammerTaked1, HammerTaked1, NewFreeCells, FreeCellsFinal).

init_transform(nord, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1, FreeCells, NewFreeCells) :-     
    sort_by_row([pos(monster_position, R, C)| Tail], State),
    transform(nord, State, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('nord'), write(NewHammerTaked), nl
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(nord, Result, Visited), !.

init_transform(sud, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1,  FreeCells, NewFreeCells) :- 
    sort_by_row([pos(monster_position, R, C)| Tail], State),
    reverse(State, ReverseState),
    transform(sud, ReverseState, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('sud'), write(NewHammerTaked), nl
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(sud, Result, Visited), !.

init_transform(ovest, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1,  FreeCells, NewFreeCells) :- 
    sort_by_column([pos(monster_position, R, C)| Tail], State),
    transform(ovest, State, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('ovest'), write(NewHammerTaked),
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(ovest, Result, Visited), !.

init_transform(est, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1,  FreeCells, NewFreeCells) :- 
    sort_by_column([pos(monster_position, R, C)| Tail], State),
    reverse(State, ReverseState),  
    %write('est'), nl,
    transform(est, ReverseState, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('est'), write(NewHammerTaked),
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(est, Result, Visited), !.

check_visited(_, [pos(monster_position, R, C) | GenState], Visited) :- 
    %write('Checking '), write(Az), nl,
    %write('visited: '), write(Visited), nl,
    %print([pos(monster_position, R, C) | GemState]), nl,
    %write('New position: '), write(pos(monster_position, R, C)), write('__'),
    sort_by_column(GenState, SortTransformedPositionGemColumn ),
    sort_by_column(SortTransformedPositionGemColumn, SortTransformedPositionGem ),
    \+ member([pos(monster_position, R, C) | SortTransformedPositionGem], Visited).
    %write(Az), write(' is valid'), nl.