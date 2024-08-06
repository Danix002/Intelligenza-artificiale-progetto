:- use_module(library(uuid)).
new_uuid(UUID) :- uuid(UUID).
:-[knowledge_example].
:-[applicable].
:-[transform].
:-[utility].

ricerca_a_star(Cammino, FinalVisited):-
    pos(monster_position, R, C),
    findall(pos(gem, RG, CG), pos(gem, RG, CG), Lpos),
    has_hammer(HammerTaked),
    pos(portal, R1, C1),
    manhattan_distance((R, R1), (C, C1), Cost),
    ampiezza_search([state([pos(monster_position, R, C) | Lpos], nothing, HammerTaked, [], 0, -1, Cost)], [], _, Cammino, FinalVisitedPosition, _),
    extract_state_from_visited(FinalVisitedPosition, FinalVisited),
    nl, nl, write('walk: '), print(Cammino), nl, nl.
    %write('gem states: '), print(GemStates), nl,
    %write('position visited by monster PRE: '), write(FinalVisitedPosition), nl,
    %write('position visited by monster: '), write(FinalVisited), nl,
    %write('hammer taked: '), print(HammerTaked1), nl,
    %write('free cells: '), print(FreeCellsFinal), nl.

extract_state_from_visited([], []).  

extract_state_from_visited([visited(State, _, _, _) | Tail], [State | TailState]):-
    extract_state_from_visited(Tail, TailState).

ampiezza_search([state([pos(monster_position, R, C) | GS], StateAction, HammerTaked, FreeCells, Name, Parent, _) | _], Visited, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal):- 
    pos(portal, R, C), HammerTaked1 is HammerTaked, FreeCellsFinal = FreeCells, 
    generate_action_path( visited([pos(monster_position, R, C) | GS], Name, Parent, StateAction), Visited, Path, FinalVisited),
    Cammino = Path, !.

ampiezza_search([state([pos(monster_position, MonsterRow, MonsterCol) | GemState], StateAction, HammerTaked, FreeCells, Name, Parent, Cost) | TailToVisit], Visited, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal):-
    %write('monster: '), print(pos(monster_position, MonsterRow, MonsterCol)), nl,
    %write('Visited: '), write(Visited), nl,
    %write('To Visit: '), write(TailToVisit), nl, 
    check_visited(_, visited([pos(monster_position, MonsterRow, MonsterCol) | GemState], Name, Parent, StateAction ), Visited),
    findall(
        Az,
        applicable(
            Az, 
            pos(monster_position, MonsterRow, MonsterCol),
            [pos(monster_position, MonsterRow, MonsterCol) | GemState],
            HammerTaked,
            FreeCells
        ),
        ActionsList
    ),
    %write('GemState: '), print(GemState), nl,
    %write('Actions: '), print(ActionsList), nl,
    %length(Visited, VLength),
    %length([state([pos(monster_position, MonsterRow, MonsterCol) | GemState], StateAction, HammerTaked, FreeCells, Name, Parent, Cost) | TailToVisit], TVLength),
    %PosLength is VLength + TVLength,
    new_uuid(UUID),
    %write('Length: '), write(PosLength), nl,
    genera_transform(state([pos(monster_position, MonsterRow, MonsterCol) | GemState], StateAction, HammerTaked, FreeCells, Name, Parent, Cost), ActionsList, NewState, Visited, UUID),
    %write('NewState: '), print(NewState), nl,
    difference(NewState, TailToVisit, StateToAdd),
    %write('StateToAdd Difference: '), print(StateToAdd), nl,
    append(TailToVisit, StateToAdd, NewTailToVisit),
    %write('NewTailToVisitWithCost: '), print(NewTailToVisitWithCost), nl,
    sort_by_euristic(NewTailToVisit, NewTailToVisitSorted),
    %write('NewTailToVisitSorted: '), print(NewTailToVisitSorted), nl, nl, nl,
    %print('NewTailToVisitSorted: '), print(NewTailToVisitSorted), nl, nl, nl,
    %write('NewTailToVisitSorted After sort: '), print(NewTailToVisitSorted), nl, nl, nl,
    sort_by_column(GemState, SortTransformedPositionGemColumn),
    sort_by_row(SortTransformedPositionGemColumn, SortTransformedPositionGem),
    ampiezza_search(NewTailToVisitSorted, [ visited([pos(monster_position, MonsterRow, MonsterCol) | SortTransformedPositionGem], Name, Parent, StateAction ) | Visited], HammerTaked1, Cammino, FinalVisited, FreeCellsFinal).

ampiezza_search([state([pos(monster_position, R, C) | GS], _, _, _, Name, Parent, _) | TailToVisit], Visited, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal):- 
    \+ check_visited(_, visited([pos(monster_position, R, C) | GS], Name, Parent), Visited),
    %print('Salto: '), print(pos(monster_position, R, C)), nl,
    ampiezza_search(TailToVisit, Visited, HammerTaked1, Cammino, FinalVisited, FreeCellsFinal).

extract_first_element([Head | _], Head).

genera_transform(_, [], [], _, _).

genera_transform(state(HeadState, StateAction, HammerTaked, FreeCells, ParentName, P, PCost), [HeadAction | TailAction], [state([pos(monster_position, Row, Col)  | TransformedPositionGem], HeadAction, HammerTaked1, NewFreeCells, Length, ParentName, Cost) | Tail], Visited, Length):-
    init_transform(HeadAction, HeadState, Visited, [pos(monster_position, Row, Col) | TransformedPositionGem], HammerTaked, HammerTaked1, FreeCells, NewFreeCells),
    new_uuid(UUID),
    pos(portal, R1, C1),
    manhattan_distance((Row, R1), (Col, C1), DCost),
    Cost is PCost + DCost,
    %write('Transform: '),  write(state([TransformedPositionMonster | TransformedPositionGem], HeadAction, HammerTaked1, NewFreeCells, Length, ParentName)), nl,
    genera_transform(state(HeadState, StateAction, HammerTaked, FreeCells, ParentName, P, PCost), TailAction, Tail, Visited, UUID ).

difference([], _, []).

difference([S | Tail], B, Risultato):-
    member(S, B), !,
    difference(Tail, B, Risultato).

difference([S | Tail], B, [S | RisTail]):-
    difference(Tail, B, RisTail).

init_transform(nord, [pos(monster_position, R, C)| Tail], _, Result, HammerTaked, HammerTaked1, FreeCells, NewFreeCells) :-     
    sort_by_row([pos(monster_position, R, C)| Tail], State),
    transform(nord, State, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('nord'), write(NewHammerTaked), nl
    move_monster_position_to_front(ResultTMP, Result),!.

init_transform(sud, [pos(monster_position, R, C)| Tail], _, Result, HammerTaked, HammerTaked1,  FreeCells, NewFreeCells) :- 
    sort_by_row([pos(monster_position, R, C)| Tail], State),
    reverse(State, ReverseState),
    %write('sud'), nl,
    transform(sud, ReverseState, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1, FreeCells, NewFreeCells),
    %write('sud'), write(NewHammerTaked), nl
    move_monster_position_to_front(ResultTMP, Result),!.

init_transform(ovest, [pos(monster_position, R, C)| Tail], _, Result, HammerTaked, HammerTaked1,  FreeCells, NewFreeCells) :- 
    sort_by_column([pos(monster_position, R, C)| Tail], State),
    %write('ovest'), nl,
    transform(ovest, State, ResultTMP, [pos(monster_position, R, C)| Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('ovest'), write(NewHammerTaked),
    move_monster_position_to_front(ResultTMP, Result), !.

init_transform(est, [pos(monster_position, R, C)| Tail], _, Result, HammerTaked, HammerTaked1,  FreeCells, NewFreeCells) :- 
    sort_by_column([pos(monster_position, R, C)| Tail], State),
    reverse(State, ReverseState),  
    %write('est'), nl,
    transform(est, ReverseState, ResultTMP, [pos(monster_position, R, C) | Tail], HammerTaked, HammerTaked1,  FreeCells, NewFreeCells),
    %write('est'), write(NewHammerTaked),
    move_monster_position_to_front(ResultTMP, Result), !.

init_transform(est, [pos(monster_position, _, _)| _], _, _, _, _,  _, _).

check_visited(_, visited([pos(monster_position, R, C) | GemState], _, _, _), Visited) :- 
    %write('Checking '), write(Az), nl,
    %write('visited: '), write(Visited), nl,
    %print([pos(monster_position, R, C) | GemState]), nl,
    %write('New position: '), write(pos(monster_position, R, C)), write('__'),
    sort_by_column(GemState, SortTransformedPositionGemColumn),
    sort_by_row(SortTransformedPositionGemColumn, SortTransformedPositionGem),
    \+ member(visited([pos(monster_position, R, C) | SortTransformedPositionGem],  _, _, _ ), Visited).
    %write(Az), write(' is valid'), nl.

extract_state_values([], []).

extract_state_values([_-State | RestPairs], [State | RestValues]) :-
    extract_state_values(RestPairs, RestValues).

abs(X, Y) :- X >= 0, Y is X.

abs(X, Y) :- X < 0, Y is -X.

manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    abs(X1 - X2, Dx),
    abs(Y1 - Y2, Dy),
    Distance is Dx + Dy.

transform_to_key_value_euristic([], []) :- true.

transform_to_key_value_euristic([state([pos(monster_position, R, C) | GemState], StateAction, HammerTaked, FreeCells, Name, Parent, Cost) | Rest], [Cost-state([pos(monster_position, R, C) | GemState], StateAction, HammerTaked, FreeCells, Name, Parent, Cost) | RestPairs]) :-
    transform_to_key_value_euristic(Rest, RestPairs).

sort_by_euristic(List, SortedList) :-
    transform_to_key_value_euristic(List, KeyValuePairs),
    keysort(KeyValuePairs, SortedKeyValuePairs),
    extract_state_values(SortedKeyValuePairs, SortedList).

generate_action_path(visited([pos(monster_position, R, C) | GS], Name, Parent, StateAction ), [visited(Pos, N, PN, PAction) | Tail], [StateAction | TPath], [visited([pos(monster_position, R, C) | GS], Name, Parent, StateAction ) | TailVisited]) :-
    Parent = N,
    generate_action_path(visited(Pos, N, PN, PAction), Tail, TPath, TailVisited).

generate_action_path(visited([pos(monster_position, R, C) | GS], Name, Parent, StateAction), [visited(_, N, _, _) | Tail], Path, Visited) :-
    Parent \= N,
    generate_action_path(visited([pos(monster_position, R, C) | GS], Name, Parent, StateAction), Tail, Path, Visited).

generate_action_path(visited([pos(monster_position, R, C) | GS], Name, -1, StateAction), [], [StateAction], [visited([pos(monster_position, R, C) | GS], Name, -1, StateAction )]) :- true.