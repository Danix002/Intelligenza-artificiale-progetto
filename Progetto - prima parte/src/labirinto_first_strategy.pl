% Definizione del labirinto
pos(wall, 0, 0).
pos(empty, 0, 1).
pos(empty, 0, 2).
pos(empty, 0, 3).
pos(empty, 0, 4).
pos(empty, 0, 5).
pos(empty, 0, 6).
pos(gem, 0, 7).
pos(empty, 0, 8).

pos(empty, 1, 0).
pos(empty, 1, 1).
pos(empty, 1, 2).
pos(empty, 1, 3).
pos(empty, 1, 4).
pos(empty, 1, 5).
pos(empty, 1, 6).
pos(empty, 1, 7).
pos(empty, 1, 8).

pos(empty, 2, 0).
pos(portal, 2, 1).
pos(empty, 2, 2).
pos(empty, 2, 3).
pos(empty, 2, 4).
pos(empty, 2, 5).
pos(empty, 2, 6).
pos(empty, 2, 7).
pos(empty, 2, 8).

pos(empty, 3, 0).
pos(empty, 3, 1).
pos(empty, 3, 2).
pos(empty, 3, 3).
pos(empty, 3, 4).
pos(destroyable_wall, 3, 5).
pos(destroyable_wall, 3, 6).
pos(destroyable_wall, 3, 7).
pos(wall, 3, 8).

pos(empty, 4, 0).
pos(empty, 4, 1).
pos(empty, 4, 2).
pos(empty, 4, 3).
pos(gem, 4, 4).
pos(destroyable_wall, 4, 5).
pos(empty, 4, 6).
pos(empty, 4, 7).
pos(empty, 4, 8).

pos(empty, 5, 0).
pos(empty, 5, 1).
pos(empty, 5, 2).
pos(empty, 5, 3).
pos(empty, 5, 4).
pos(destroyable_wall, 5, 5).
pos(empty, 5, 6).
pos(empty, 5, 7).
pos(empty, 5, 8).

pos(empty, 6, 0).
pos(empty, 6, 1).
pos(empty, 6, 2).
pos(hammer, 6, 3).
pos(empty, 6, 4).
pos(destroyable_wall, 6, 5).
pos(empty, 6, 6).
pos(monster_position, 6, 7).
pos(empty, 6, 8).

pos(empty, 7, 0).
pos(gem, 7, 1).
pos(empty, 7, 2).
pos(empty, 7, 3).
pos(empty, 7, 4).
pos(destroyable_wall, 7, 5).
pos(empty, 7, 6).
pos(empty, 7, 7).
pos(empty, 7, 8).

pos(empty, 8, 0).
pos(empty, 8, 1).
pos(empty, 8, 2).
pos(empty, 8, 3).
pos(gem, 8, 4).
pos(destroyable_wall, 8, 5).
pos(empty, 8, 6).
pos(hammer, 8, 7).
pos(empty, 8, 8).

azione(nord).
azione(sud).
azione(est).
azione(ovest).

has_hammer(0).

applicable(nord, pos(monster_position, R, C), [_ | GemState], _, _) :-
    R > 0,
    R1 is R - 1,
    \+ member(pos(gem, R1, C), GemState),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C).

% caso in cui c'è un muro distruttibile non ancora distrutto e il mostro ha il martello
applicable(nord, pos(monster_position, R, C), [_ | GemState], HammerTaked, FreeCells) :-
    R > 0,
    R1 is R - 1,
    pos(destroyable_wall, R1, C),
    \+ member(pos(empty, R1, C), FreeCells),
    \+ member(pos(gem, R1, C), GemState),
    \+ pos(wall, R1, C),
    HammerTaked > 0.

% caso in cui c'è un muro distruttibile ma già distrutto
applicable(nord, pos(monster_position, R, C), [_ | GemState], _, FreeCells) :-
    R > 0,
    R1 is R - 1,
    pos(destroyable_wall, R1, C),
    \+ member(pos(gem, R1, C), GemState),
    \+ pos(wall, R1, C),
    member(pos(empty, R1, C), FreeCells).

applicable(nord, pos(monster_position, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    R > 0,
    R1 is R - 1,
    member(pos(gem, R1, C), GemState),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    applicable(nord, pos(gem, R1, C), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(nord, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    R > 0,
    R1 is R-1,
    \+ member(pos(gem, R1, C) , GemState),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    \+ pos(portal, R1, C).

applicable(nord, pos(gem, R, C), [MonsterState | GemState], _, FreeCells) :-
    R > 0,
    R1 is R-1,
    \+ member(pos(gem, R1, C) , GemState),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    pos(destroyable_wall, R1, C),
    member(pos(empty, R1, C), FreeCells),
    \+ pos(portal, R1, C).

applicable(nord, pos(gem, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    R > 0,
    R1 is R-1,
    member(pos(gem, R1, C) , GemState),
    MonsterState \= pos(monster_position, R1, C),
    applicable(nord, pos(gem, R1, C), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(est, pos(monster_position, R, C), [_ | GemState], _, _) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).

applicable(est, pos(monster_position, R, C), [_ | GemState], HammerTaked, FreeCells) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    pos(destroyable_wall, R, C1),
    \+ member(pos(empty, R, C1), FreeCells),
    HammerTaked > 0.

applicable(est, pos(monster_position, R, C), [_ | GemState], _, FreeCells) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    pos(destroyable_wall, R, C1),
    member(pos(empty, R, C1), FreeCells).

applicable(est, pos(monster_position, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    C < 7,
    C1 is C + 1,
    member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    applicable(est, pos(gem, R, C1), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(est, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    \+ pos(portal, R, C1).  

applicable(est, pos(gem, R, C), [MonsterState | GemState], _, FreeCells) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    pos(destroyable_wall, R, C1),
    member(pos(empty, R, C1), FreeCells),
    \+ pos(portal, R, C1). 

applicable(est, pos(gem, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    C < 7,
    C1 is C + 1,
    member(pos(gem, R, C1), GemState),
    applicable(est, pos(gem, R, C1), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(sud, pos(monster_position, R, C), [_ | GemState], _, _) :-
    R < 7,
    R1 is R + 1,
    \+ member(pos(gem, R1, C) , GemState),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C).

applicable(sud, pos(monster_position, R, C), [_ | GemState], HammerTaked, FreeCells) :-
    R < 7,
    R1 is R + 1,
    \+ member(pos(gem, R1, C), GemState),
    \+ pos(wall, R1, C),
    pos(destroyable_wall, R1, C),
    \+ member(pos(empty, R1, C), FreeCells),
    HammerTaked > 0.

applicable(sud, pos(monster_position, R, C), [_ | GemState], _, FreeCells) :-
    R < 7,
    R1 is R + 1,
    \+ member(pos(gem, R1, C), GemState),
    \+ pos(wall, R1, C),
    pos(destroyable_wall, R1, C),
    member(pos(empty, R1, C), FreeCells).
 
applicable(sud, pos(monster_position, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    R < 7,
    R1 is R + 1,
    member(pos(gem, R1, C) , GemState),
    \+ pos(wall, R1, C),
    pos(destroyable_wall, R1, C),
    member(pos(empty, R1, C), FreeCells),
    applicable(sud, pos(gem, R1, C), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(sud, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    R < 7,
    R1 is R + 1,
    \+ member(pos(gem, R1, C), GemState),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    \+ pos(portal, R1, C).

applicable(sud, pos(gem, R, C), [MonsterState | GemState], _, FreeCells) :-
    R < 7,
    R1 is R + 1,
    \+ member(pos(gem, R1, C), GemState),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    pos(destroyable_wall, R1, C),
    member(pos(empty, R1, C), FreeCells),
    \+ pos(portal, R1, C).

applicable(sud, pos(gem, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    R < 7,
    R1 is R + 1,
    member( pos(gem, R1, C) , GemState ),
    MonsterState \= pos(monster_position, R1, C),
    applicable(sud, pos(gem, R1, C), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(ovest, pos(monster_position, R,C), [_ | GemState], _, _) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).

applicable(ovest, pos(monster_position, R,C), [_ | GemState], HammerTaked, FreeCells) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    pos(destroyable_wall, R, C1),
    \+ member(pos(empty, R, C1), FreeCells),
    HammerTaked > 0.

applicable(ovest, pos(monster_position, R,C), [_ | GemState], _, FreeCells) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    pos(destroyable_wall, R, C1),
    member(pos(empty, R, C1), FreeCells).

applicable(ovest, pos(monster_position, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    C > 0,
    C1 is C - 1,
    member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    applicable(ovest, pos(gem, R, C1), [MonsterState | GemState], HammerTaked, FreeCells).

applicable(ovest, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    \+ pos(portal, R, C1).

applicable(ovest, pos(gem, R, C), [MonsterState | GemState], _, FreeCells) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    pos(destroyable_wall, R, C1),
    member(pos(empty, R, C1), FreeCells),
    \+ pos(portal, R, C1).

applicable(ovest, pos(gem, R, C), [MonsterState | GemState], HammerTaked, FreeCells) :-
    C > 0,
    C1 is C - 1,
    member(pos(gem, R, C1) , GemState),
    applicable(ovest, pos(gem, R, C1), [MonsterState | GemState], HammerTaked, FreeCells).

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
    
transform(nord, [pos(T, R, C)| Tail], [ HP | TP], State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    det_position_nord(pos(T, R, C), R1, State, HammerTaked, NewHammerTaked,  FreeCells, NewFreeCells),
    HP = pos(T, R1, C),
    TMP = pos(T, R, C),
    update_value_in_list(HP, TMP, State, NewState),
    transform(nord, Tail, TP, NewState, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

transform(nord, [], [], _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- HammerTaked1 is HammerTaked, FreeCellsTMP = FreeCells, true.

transform(sud, [pos(T, R, C)| Tail], [ HP | TP], State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    det_position_sud(pos(T, R, C), R1, State, HammerTaked, NewHammerTaked, FreeCells, NewFreeCells),
    HP = pos(T, R1, C),
    TMP = pos(T, R, C),
    update_value_in_list(HP, TMP, State, NewState),
    transform(sud, Tail, TP, NewState, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

transform(sud, [], [], _,  HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- HammerTaked1 is HammerTaked, FreeCellsTMP = FreeCells, true.

transform(ovest, [pos(T, R, C)| Tail], [ HP | TP], State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    det_position_ovest(pos(T, R, C), C1, State, HammerTaked, NewHammerTaked, FreeCells, NewFreeCells),
    HP = pos(T, R, C1),
    TMP = pos(T, R, C),
    update_value_in_list(HP, TMP, State, NewState),
    transform(ovest, Tail, TP, NewState, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

transform(ovest, [], [], _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- HammerTaked1 is HammerTaked, FreeCellsTMP = FreeCells, true.

transform(est, [pos(T, R, C)| Tail], [HP | TP], State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    det_position_est(pos(T, R, C), C1, State, HammerTaked, NewHammerTaked, FreeCells, NewFreeCells),
    HP = pos(T, R, C1),
    TMP = pos(T, R, C),
    update_value_in_list(pos(T, R, C1), TMP, State, NewState),
    transform(est, Tail, TP, NewState, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).
    
transform(est, [], [], _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- HammerTaked1 is HammerTaked, FreeCellsTMP = FreeCells, true.

det_position_nord(pos(monster_position, R, C), R1, _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    pos(portal, R, C),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    R1 is R.

det_position_nord(pos(monster_position, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(nord, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    RTMP is R - 1,
    pos(hammer, RTMP, C),
    \+ member(pos(empty, RTMP, C), FreeCells),
    NewHammerTaked is HammerTaked + 1,
    append(FreeCells, [pos(empty, RTMP, C)], NewFreeCells),
    det_position_nord(pos(monster_position, RTMP, C), R1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_nord(pos(monster_position, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(nord, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    RTMP is R - 1,
    pos(destroyable_wall, RTMP, C),
    \+ member(pos(empty, RTMP, C), FreeCells),
    NewHammerTaked is HammerTaked - 1,
    append(FreeCells, [pos(empty, RTMP, C)], NewFreeCells),
    det_position_nord(pos(monster_position, RTMP, C), R1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_nord(pos(T, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    \+ applicable(nord, pos(T, R,C), State, HammerTaked, FreeCells),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    R1 is R.

det_position_nord(pos(T, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :- 
    applicable(nord, pos(T, R,C), State, HammerTaked, FreeCells),
    RTMP is R - 1,
    det_position_nord(pos(T, RTMP, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP).

det_position_sud(pos(T, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    \+ applicable(sud, pos(T, R,C), State, HammerTaked, FreeCells),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    R1 is R.

det_position_sud(pos(monster_position, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(sud, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    RTMP is R + 1,
    pos(hammer, RTMP, C),
    \+ member(pos(empty, RTMP, C), FreeCells),
    NewHammerTaked is HammerTaked + 1,
    append(FreeCells, [pos(empty, RTMP, C)], NewFreeCells),
    det_position_sud(pos(monster_position, RTMP, C), R1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_sud(pos(monster_position, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(sud, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    RTMP is R + 1,
    pos(destroyable_wall, RTMP, C),
    \+ member(pos(empty, RTMP, C), FreeCells),
    NewHammerTaked is HammerTaked - 1,
    append(FreeCells, [pos(empty, RTMP, C)], NewFreeCells),
    det_position_sud(pos(monster_position, RTMP, C), R1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_sud(pos(_, R, C), R1, _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    pos(portal, R, C),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    R1 is R.

det_position_sud(pos(T, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(sud, pos(T, R,C), State, HammerTaked, FreeCells),
    RTMP is R + 1,
    det_position_sud(pos(T, RTMP, C), R1, State,  HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP).

det_position_ovest(pos(T, R, C), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    \+ applicable(ovest, pos(T, R, C), State, HammerTaked, FreeCells),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    C1 is C.

det_position_ovest(pos(monster_position, R, C), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(ovest, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    CTMP is C - 1,
    pos(hammer, R, CTMP),
    NewHammerTaked is HammerTaked + 1,
    \+ member(pos(empty, R, CTMP), FreeCells),
    append(FreeCells, [pos(empty, R, CTMP)], NewFreeCells),
    det_position_ovest(pos(monster_position, R, CTMP), C1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_ovest(pos(monster_position, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(ovest, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    CTMP is C - 1,
    pos(destroyable_wall, R, CTMP),
    \+ member(pos(empty, R, CTMP), FreeCells),
    NewHammerTaked is HammerTaked - 1,
    append(FreeCells, [pos(empty, R, CTMP)], NewFreeCells),
    det_position_ovest(pos(monster_position, R, CTMP), R1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_ovest(pos(monster_position, R, C), C1, _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    pos(portal, R, C),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    C1 is C.

det_position_ovest(pos(T, R, C), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(ovest, pos(T, R, C), State, HammerTaked, FreeCells),
    CTMP is C - 1,
    det_position_ovest(pos(T, R, CTMP), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP).

det_position_est(pos(T, R, C), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    \+ applicable(est, pos(T, R, C), State, HammerTaked, FreeCells),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    C1 is C.

det_position_est(pos(monster_position, R, C), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(est, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    CTMP is C + 1,
    pos(hammer, R, CTMP),
    \+ member(pos(empty, R, CTMP), FreeCells),
    NewHammerTaked is HammerTaked + 1,
    append(FreeCells, [pos(empty, R, CTMP)], NewFreeCells),
    det_position_est(pos(monster_position, R, CTMP), C1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_est(pos(monster_position, R, C), R1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(est, pos(monster_position, R,C), State, HammerTaked, FreeCells),
    CTMP is C + 1,
    pos(destroyable_wall, R, CTMP),
    \+ member(pos(empty, R, CTMP), FreeCells),
    NewHammerTaked is HammerTaked - 1,
    append(FreeCells, [pos(empty, R, CTMP)], NewFreeCells),
    det_position_est(pos(monster_position, R, CTMP), R1, State, NewHammerTaked, HammerTaked1, NewFreeCells, FreeCellsTMP).

det_position_est(pos(monster_position, R, C), C1, _, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    pos(portal, R, C),
    HammerTaked1 is HammerTaked,
    FreeCellsTMP = FreeCells,
    C1 is C.

det_position_est(pos(T, R, C), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP) :-
    applicable(est, pos(T, R, C), State, HammerTaked, FreeCells),
    CTMP is C + 1,
    det_position_est(pos(T, R, CTMP), C1, State, HammerTaked, HammerTaked1, FreeCells, FreeCellsTMP).

extract_values([], []) :- true.

extract_values([_-Value | RestPairs], [Value | RestValues]) :-
    extract_values(RestPairs, RestValues).

transform_to_key_value_column([], []) :- true.

transform_to_key_value_column([pos(T, R, C) | Rest], [C-pos(T, R, C) | RestPairs]) :-
    transform_to_key_value_column(Rest, RestPairs).

sort_by_column(List, SortedList) :-
    transform_to_key_value_column(List, KeyValuePairs),
    keysort(KeyValuePairs, SortedKeyValuePairs),
    extract_values(SortedKeyValuePairs, SortedList).

transform_to_key_value_row([], []) :- true.

transform_to_key_value_row([pos(T, R, C) | Rest], [R-pos(T, R, C) | RestPairs]) :-
    transform_to_key_value_row(Rest, RestPairs).

sort_by_row(List, SortedList) :-
    transform_to_key_value_row(List, KeyValuePairs),
    keysort(KeyValuePairs, SortedKeyValuePairs),
    extract_values(SortedKeyValuePairs, SortedList).

move_monster_position_to_front(List, Result) :-
    partition(monster_position_filter, List, MonsterPositions, OtherPositions),
    append(MonsterPositions, OtherPositions, Result).

monster_position_filter(pos(monster_position, _, _)) :- true.

update_value_in_list(_, _, [], []) :- true.

update_value_in_list(pos(NewT, NewR, NewC), pos(OldT, OldR, OldC), [pos(OldT, OldR, OldC) | OldTail], [pos(NewT, NewR, NewC) | NewTail]) :-
    update_value_in_list(pos(NewT, NewR, NewC), pos(OldT, OldR, OldC), OldTail, NewTail).

update_value_in_list(pos(NewT, NewR, NewC), pos(OldT, OldR, OldC), [pos(T, R, C) | OldTail], [pos(T, R, C) | NewTail]) :-
    (T \= OldT; R \= OldR; C \= OldC),
    update_value_in_list(pos(NewT, NewR, NewC), pos(OldT, OldR, OldC), OldTail, NewTail).

