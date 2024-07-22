:- discontiguous pos/3.
:- discontiguous has_hammer/1.
:- discontiguous initTransform/3.
:- discontiguous transform/4.

% Definizione del labirinto
pos(wall, 0, 0).
pos(hammer, 0, 1).
pos(hammer, 0, 2).
pos(empty, 0, 3).
pos(empty, 0, 4).
pos(empty, 0, 5).
pos(empty, 0, 6).
pos(gem, 0, 7).

pos(empty, 1, 0).
pos(hammer, 1, 1).
pos(empty, 1, 2).
pos(empty, 1, 3).
pos(wall, 1, 4).
pos(empty, 1, 5).
pos(empty, 1, 6).
pos(empty, 1, 7).

pos(empty, 2, 0).
pos(portal, 2, 1).
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
pos(gem, 4, 4).
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
pos(destroyable_wall, 6, 1).
pos(destroyable_wall, 6, 2).
pos(destroyable_wall, 6, 3).
pos(wall, 6, 4).
pos(empty, 6, 5).
pos(empty, 6, 6).
pos(monster_position, 6, 7).

pos(wall, 7, 0).
pos(gem, 7, 1).
pos(empty, 7, 2).
pos(empty, 7, 3).
pos(wall, 7, 4).
pos(wall, 7, 5).
pos(empty, 7, 6).
pos(empty, 7, 7).

azione(nord).
azione(sud).
azione(est).
azione(ovest).

has_hammer(0).

% TODO DISTRUTTIBLE WALL
applicable(nord, pos(monster_position, R, C), [_ | GemState], _, _) :-
    R > 0,
    R1 is R - 1,
    \+ member(pos(gem, R1, C) , GemState),
    \+ pos(wall, R1, C),
    \+ pos(hammer, R1, C),
    \+ pos(destroyable_wall, R1, C).
    
applicable(nord, pos(monster_position, R, C), [MonsterState | GemState], _, _) :-
    R > 0,
    R1 is R - 1,
    member(pos(gem, R1, C) , GemState),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    applicable(nord, pos(gem, R1, C), [MonsterState | GemState], _, _).

applicable(nord, pos(monster_position, R, C), [_ | _], HammerTaked, HammerTaked1) :-
    R > 0,
    R1 is R - 1,
    pos(hammer, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    HammerTaked1 is HammerTaked + 1.

applicable(nord, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    R > 0,
    R1 is R-1,
    \+ member(pos(gem, R1, C) , GemState),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    \+ pos(portal, R1, C).

applicable(nord, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    R > 0,
    R1 is R-1,
    member(pos(gem, R1, C) , GemState),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    \+ pos(portal, R1, C),
    applicable(nord, pos(gem, R1, C), [MonsterState | GemState], _, _).

applicable(est, pos(monster_position, R, C), [_ | GemState], _, _) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(hammer, R, C1),
    \+ pos(destroyable_wall, R, C1).

applicable(est, pos(monster_position, R, C), [MonsterState | GemState], _, _) :-
    C < 7,
    C1 is C + 1,
    member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    applicable(est, pos(gem, R, C1), [MonsterState | GemState], _, _).

applicable(est, pos(monster_position, R, C), [_ | _], HammerTaked, HammerTaked1) :-
    C < 7,
    C1 is C + 1,
    pos(hammer, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    HammerTaked1 is HammerTaked + 1.

applicable(est, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    C < 7,
    C1 is C + 1,
    \+ member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    \+ pos(portal, R, C1).  

applicable(est, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    C < 7,
    C1 is C + 1,
    member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    \+ pos(portal, R, C1),
    applicable(est, pos(gem, R, C1), [MonsterState | GemState], _, _).

applicable(sud, pos(monster_position, R, C), [_ | _], HammerTaked, HammerTaked1) :-
    R < 7,
    R1 is R + 1,
    pos(hammer, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    HammerTaked1 is HammerTaked + 1.

applicable(sud, pos(monster_position, R, C), [_ | GemState], _, _) :-
    R < 7,
    R1 is R + 1,
    \+ member(pos(gem, R1, C) , GemState),
    \+ pos(wall, R1, C),
    \+ pos(hammer, R1, C),
    \+ pos(destroyable_wall, R1, C).
 
applicable(sud, pos(monster_position, R, C), [MonsterState | GemState], _, _) :-
    R < 7,
    R1 is R + 1,
    member(pos(gem, R1, C) , GemState),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    applicable(sud, pos(gem, R1, C), [MonsterState | GemState], _, _).

applicable(sud, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    R < 7,
    R1 is R + 1,
    \+ member( pos(gem, R1, C) , GemState ),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    \+ pos(portal, R1, C).

applicable(sud, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    R < 7,
    R1 is R + 1,
    member( pos(gem, R1, C) , GemState ),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C),
    \+ pos(portal, R1, C),
    applicable(sud, pos(gem, R1, C), [MonsterState | GemState], _, _).

applicable(ovest, pos(monster_position, R,C), [_ | GemState], _, _) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    \+ pos(hammer, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).

applicable(ovest, pos(monster_position, R, C), [MonsterState | GemState], _, _) :-
    C > 0,
    C1 is C - 1,
    member(pos(gem, R, C1) , GemState),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    applicable(ovest, pos(gem, R, C1), [MonsterState | GemState], _, _).

applicable(ovest, pos(monster_position, R, C), [_ | _], HammerTaked, HammerTaked1) :-
    C > 0,
    C1 is C - 1,
    pos(hammer, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    HammerTaked1 is HammerTaked + 1.

applicable(ovest, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    C > 0,
    C1 is C - 1,
    \+ member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    \+ pos(portal, R, C1).

applicable(ovest, pos(gem, R, C), [MonsterState | GemState], _, _) :-
    C > 0,
    C1 is C - 1,
    member(pos(gem, R, C1) , GemState),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1),
    \+ pos(portal, R, C1),
    applicable(ovest, pos(gem, R, C1), [MonsterState | GemState], _, _).

ricerca(Cammino, GemStates, FinalVisited):-
    pos(monster_position, R, C),
    findall(pos(gem, RG, CG), pos(gem, RG, CG), Lpos),
    has_hammer(HammerTaked),
    profondity_search(pos(monster_position, R, C), Lpos, Cammino, [pos(monster_position, R, C)],  GemStates, FinalVisited, HammerTaked, HammerTaked1),
    write('gem states: '), print(GemStates), nl,
    write('position visited by monster: '), print(FinalVisited), nl,
    write('hammer taked: '), print(HammerTaked1), nl,
    write('walk: '), print(Cammino), nl.

profondity_search(pos(monster_position, MonsterRow, MonsterCol), GemState, [], Visited, [GemState|[]], FinalVisited, _, _) :- pos(portal, MonsterRow, MonsterCol), FinalVisited = Visited, !.

profondity_search(pos(monster_position, MonsterRow, MonsterCol), GemState, [Az|SeqAzioni], Visited, [GemState| Tail], FinalVisited, HammerTaked, HammerTaked1) :-
    applicable(
        Az, 
        pos(monster_position, MonsterRow, MonsterCol),
        [pos(monster_position, MonsterRow, MonsterCol) | GemState],
        HammerTaked,
        HammerTaked1
    ),
    init_transform(Az, [pos(monster_position, MonsterRow, MonsterCol) | GemState], Visited, [TransformedPositionMonster | TransformedPositionGem], HammerTaked, HammerTaked1),
    profondity_search(TransformedPositionMonster, TransformedPositionGem, SeqAzioni, [TransformedPositionMonster | Visited], Tail, FinalVisited, HammerTaked, HammerTaked1).

init_transform(nord, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1) :-     
    sort_by_row([pos(monster_position, R, C)| Tail], State),
    transform(nord, State, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1),
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(nord, Result, Visited), !.

init_transform(sud, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1) :- 
    sort_by_row([pos(monster_position, R, C)| Tail], State),
    reverse(State, ReverseState),
    transform(sud, ReverseState, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1),
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(sud, Result, Visited), !.

init_transform(ovest, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1) :- 
    sort_by_column([pos(monster_position, R, C)| Tail], State),
    transform(ovest, State, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1),
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(ovest, Result, Visited), !.

init_transform(est, [pos(monster_position, R, C)| Tail], Visited, Result, HammerTaked, HammerTaked1) :- 
    sort_by_column([pos(monster_position, R, C)| Tail], State),
    reverse(State, ReverseState),   
    transform(est, ReverseState, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1),
    move_monster_position_to_front(ResultTMP, Result),
    check_visited(est, Result, Visited), !.

check_visited(_, [pos(monster_position, R, C) | _], Visited) :- 
    %write('Checking '), write(Az), nl,
    %write('visited: '), write(Visited), nl,
    %print([pos(monster_position, R, C) | GemState]), nl,
    %write('New position: '), write(pos(monster_position, R, C)), write('__'),
    \+ member(pos(monster_position, R, C), Visited).
    %write(Az), write(' is valid'), nl.
    
transform(nord, [pos(T, R, C)| Tail], [ HP | TP], State, HammerTaked, HammerTaked1) :- 
    det_position_nord(pos(T, R, C), R1, State, HammerTaked, HammerTaked1),
    HP = pos(T, R1, C),
    TMP = pos(T, R, C),
    update_value_in_list(HP, TMP, State, NewState),
    transform(nord, Tail, TP, NewState, HammerTaked, HammerTaked1).

transform(nord, [], [], _, _, _) :- true.

transform(sud, [pos(T, R, C)| Tail], [ HP | TP], State, HammerTaked, HammerTaked1) :- 
    det_position_sud(pos(T, R, C), R1, State, HammerTaked, HammerTaked1),
    HP = pos(T, R1, C),
    TMP = pos(T, R, C),
    update_value_in_list(HP, TMP, State, NewState),
    transform(sud, Tail, TP, NewState, HammerTaked, HammerTaked1).

transform(sud, [], [], _, _, _):- true.

transform(ovest, [pos(T, R, C)| Tail], [ HP | TP], State, HammerTaked, HammerTaked1) :- 
    det_position_ovest(pos(T, R, C), C1, State, HammerTaked, HammerTaked1),
    HP = pos(T, R, C1),
    TMP = pos(T, R, C),
    update_value_in_list(HP, TMP, State, NewState),
    transform(ovest, Tail, TP, NewState, HammerTaked, HammerTaked1).

transform(ovest, [], [], _, _, _) :- true.

transform(est, [pos(T, R, C)| Tail], [HP | TP], State, HammerTaked, HammerTaked1) :- 
    det_position_est(pos(T, R, C), C1, State, HammerTaked, HammerTaked1),
    HP = pos(T, R, C1),
    TMP = pos(T, R, C),
    update_value_in_list(pos(T, R, C1), TMP, State, NewState),
    transform(est, Tail, TP, NewState, HammerTaked, HammerTaked1).
    
transform(est, [], [], _, _, _) :- true.

/**
det_position_nord(pos(monster_position, R, C), R1) :- 
    R > 0,
    RTMP is R - 1,
    pos(destroyable_wall, RTMP, C),
    has_hammer(N),
    N > 0,
    det_position_nord(pos(monster_position, RTMP, C), R1).
*/

det_position_nord(pos(monster_position, R, C), R1, _, _, _) :- 
    pos(portal, R, C),
    R1 is R.

det_position_nord(pos(T, R, C), R1, State, HammerTaked, HammerTaked1) :- 
    \+ applicable(nord, pos(T, R,C), State, HammerTaked, HammerTaked1),
    R1 is R.

det_position_nord(pos(T, R, C), R1, State, HammerTaked, HammerTaked1) :- 
    applicable(nord, pos(T, R,C), State, HammerTaked, HammerTaked1),
    RTMP is R - 1,
    det_position_nord(pos(T, RTMP, C), R1, State, HammerTaked, HammerTaked1).

/**
det_position_nord(pos(gem, R, C), R1) :- 
    applicable(nord, pos(gem, R,C), [ _ | GemState]),
    % controll hammer
    det_position_nord(pos(_, RTMP, C), R1).

det_position_nord(pos(monster_position, R, C), R1) :- 
    R > 0,
    RTMP is R - 1,
    pos(hammer, RTMP, C),
    det_position_nord(pos(_, RTMP, C), R1).

det_position_sud(pos(monster_position, R, C), R1) :- 
    R < 7,
    RTMP is R + 1,
    pos(destroyable_wall, RTMP, C),
    has_hammer(N),
    N > 0,
    det_position_sud(pos(monster_position, RTMP, C), R1).
**/

det_position_sud(pos(T, R, C), R1, State, HammerTaked, HammerTaked1) :-
    \+ applicable(sud, pos(T, R,C), State, HammerTaked, HammerTaked1),
    R1 is R.

det_position_sud(pos(_, R, C), R1, _, _, _) :-
    pos(portal, R, C),
    R1 is R.

/**
det_position_sud(pos(gem, R, C), R1) :-
    R < 7,
    RTMP is R + 1,
    pos(hammer, RTMP, C),
    det_position_sud(pos(_, RTMP, C), R1).

det_position_sud(pos(monster_position, R, C), R1) :-
    R < 7,
    RTMP is R + 1,
    pos(hammer, RTMP, C),
    det_position_sud(pos(_, RTMP, C), R1).
*/

det_position_sud(pos(T, R, C), R1, State, HammerTaked, HammerTaked1) :-
    applicable(sud, pos(T, R,C), State, HammerTaked, HammerTaked1),
    RTMP is R + 1,
    det_position_sud(pos(T, RTMP, C), R1, State, HammerTaked, HammerTaked1).

/**
det_position_ovest(pos(monster_position, R, C), C1) :- 
    C > 0,
    CTMP is C - 1,
    pos(destroyable_wall, R, CTMP),
    has_hammer(N),
    N > 0,
    det_position_ovest(pos(monster_position, R, CTMP), C1).
*/

det_position_ovest(pos(T, R, C), C1, State, HammerTaked, HammerTaked1) :-
    \+ applicable(ovest, pos(T, R, C), State, HammerTaked, HammerTaked1),
    C1 is C.


det_position_ovest(pos(monster_position, R, C), C1, _, _, _) :-
    pos(portal, R, C),
    C1 is C.

/**
det_position_ovest(pos(gem, R, C), C1) :-
    C > 0,
    CTMP is C - 1,
    pos(hammer, R, CTMP),
    det_position_ovest(pos(_, R, CTMP), C1).

det_position_ovest(pos(monster_position, R, C), C1) :-
    C > 0,
    CTMP is C - 1,
    pos(hammer, R, CTMP),
    det_position_ovest(pos(_, R, CTMP), C1).
*/

det_position_ovest(pos(T, R, C), C1, State, HammerTaked, HammerTaked1) :-
    applicable(ovest, pos(T, R, C), State, HammerTaked, HammerTaked1),
    CTMP is C - 1,
    det_position_ovest(pos(T, R, CTMP), C1, State, HammerTaked, HammerTaked1).

/**
det_position_est(pos(monster_position, R, C), C1) :-
    C < 7,
    CTMP is C + 1,
    pos(destroyable_wall, R, CTMP),
    has_hammer(N),
    N > 0,
    det_position_est(pos(monster_position, R, CTMP), C1).
*/

det_position_est(pos(T, R, C), C1, State, HammerTaked, HammerTaked1) :-
    \+ applicable(est, pos(T, R, C), State, HammerTaked, HammerTaked1),
    C1 is C.

/**
det_position_est(pos(gem, R, C), C1) :-
    C < 7,
    CTMP is C + 1,
    pos(hammer, R, CTMP),
    det_position_est(pos(_, R, CTMP), C1).

det_position_est(pos(monster_position, R, C), C1) :-
    C < 7,
    CTMP is C + 1,
    pos(hammer, R, CTMP),
    det_position_est(pos(_, R, CTMP), C1).
*/

det_position_est(pos(monster_position, R, C), C1, _, _, _) :-
    pos(portal, R, C),
    C1 is C.

det_position_est(pos(T, R, C), C1, State, HammerTaked, HammerTaked1) :-
    applicable(est, pos(T, R, C), State, HammerTaked, HammerTaked1),
    CTMP is C + 1,
    det_position_est(pos(T, R, CTMP), C1, State, HammerTaked, HammerTaked1).

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

