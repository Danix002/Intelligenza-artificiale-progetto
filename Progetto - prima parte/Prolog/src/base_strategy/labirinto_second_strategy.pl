:- use_module(library(uuid)).
new_uuid(UUID) :- uuid(UUID).

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

check_visited(_, visited(pos(monster_position, R, C), _, _, _), Visited) :- 
    \+ member(visited(pos(monster_position, R, C),  _, _, _ ), Visited).


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


ricerca_a_star(Cammino, FinalVisited):-
    pos(monster_position, R, C),
    pos(portal, R1, C1),
    manhattan_distance((R, R1), (C, C1), Cost),
    ampiezza_search([state(pos(monster_position, R, C), nothing, 0, -1, Cost)], [], Cammino, FinalVisitedPosition),
    extract_state_from_visited(FinalVisitedPosition, FinalVisited),
    nl, nl, write('walk: '), print(Cammino), nl, nl.
    %write('position visited by monster PRE: '), write(FinalVisitedPosition), nl,
    %write('position visited by monster: '), write(FinalVisited), nl.

extract_state_from_visited([], []).  

extract_state_from_visited([visited(State, _, _, _) | Tail], [State | TailState]):-
    extract_state_from_visited(Tail, TailState).

ampiezza_search([state(pos(monster_position, R, C), StateAction, Name, Parent, _) | _], Visited, Cammino, FinalVisited):- 
    pos(portal, R, C),
    generate_action_path( visited(pos(monster_position, R, C), Name, Parent, StateAction), Visited, Path, FinalVisited),
    Cammino = Path, !.

ampiezza_search([state(pos(monster_position, MonsterRow, MonsterCol), StateAction, Name, Parent, Cost) | TailToVisit], Visited, Cammino, FinalVisited):-
    check_visited(_, visited(pos(monster_position, MonsterRow, MonsterCol), Name, Parent, StateAction ), Visited),
    findall(
        Az,
        applicable(Az, pos(monster_position, MonsterRow, MonsterCol)),
        ActionsList
    ),
    %write(ActionsList), nl,
    new_uuid(UUID),
    %write('Prima di genera: '), write(pos(monster_position, MonsterRow, MonsterCol)), nl,
    genera_transform(state(pos(monster_position, MonsterRow, MonsterCol), StateAction, Name, Parent, Cost), ActionsList, NewState, Visited, UUID),
    %write('NewState: '), print(NewState), nl,
    difference(NewState, TailToVisit, StateToAdd),
    %write('StateToAdd Difference: '), print(StateToAdd), nl,
    append(TailToVisit, StateToAdd, NewTailToVisit),
    sort_by_euristic(NewTailToVisit, NewTailToVisitSorted),
    %write('sort euristiv:'), write(NewTailToVisitSorted),
    ampiezza_search(NewTailToVisitSorted, [visited(pos(monster_position, MonsterRow, MonsterCol), Name, Parent, StateAction ) | Visited], Cammino, FinalVisited).

ampiezza_search([state(pos(monster_position, R, C), _, Name, Parent, _) | TailToVisit], Visited, Cammino, FinalVisited):- 
    \+ check_visited(_, visited(pos(monster_position, R, C), Name, Parent), Visited),
    %print('Salto: '), print(pos(monster_position, R, C)), nl,
    ampiezza_search(TailToVisit, Visited, Cammino, FinalVisited).


genera_transform(_, [], [], _, _).

genera_transform(state(pos(monster_position, MonsterRow, MonsterCol), StateAction, ParentName, P, PCost), [HeadAction | TailAction], [state(pos(monster_position, Row, Col), HeadAction, Length, ParentName, Cost) | Tail], Visited, Length):-
    %write('dentro genera: '), write(pos(monster_position, MonsterRow, MonsterCol)), nl,
    transform(HeadAction, pos(monster_position, MonsterRow, MonsterCol), pos(monster_position, Row, Col)),
    new_uuid(UUID),
    pos(portal, R1, C1),
    manhattan_distance((Row, R1), (Col, C1), DCost),
    Cost is PCost + DCost,
    genera_transform(state(pos(monster_position, MonsterRow, MonsterCol), StateAction, ParentName, P, PCost), TailAction, Tail, Visited, UUID ).

difference([], _, []).

difference([S | Tail], B, Risultato):-
    member(S, B), !,
    difference(Tail, B, Risultato).

difference([S | Tail], B, [S | RisTail]):-
    difference(Tail, B, RisTail).

manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    abs(X1 - X2, Dx),
    abs(Y1 - Y2, Dy),
    Distance is Dx + Dy.

generate_action_path(visited(pos(monster_position, R, C), Name, Parent, StateAction ), [visited(Pos, N, PN, PAction) | Tail], [StateAction | TPath], [visited(pos(monster_position, R, C), Name, Parent, StateAction ) | TailVisited]) :-
    Parent = N,
    generate_action_path(visited(Pos, N, PN, PAction), Tail, TPath, TailVisited).

generate_action_path(visited(pos(monster_position, R, C), Name, Parent, StateAction), [visited(_, N, _, _) | Tail], Path, Visited) :-
    Parent \= N,
    generate_action_path(visited(pos(monster_position, R, C), Name, Parent, StateAction), Tail, Path, Visited).

generate_action_path(visited(pos(monster_position, R, C), Name, -1, StateAction), [], [StateAction], [visited(pos(monster_position, R, C), Name, -1, StateAction )]) :- true.

transform_to_key_value_euristic([], []) :- true.

transform_to_key_value_euristic([state(pos(monster_position, R, C), StateAction, Name, Parent, Cost) | Rest], [Cost-state(pos(monster_position, R, C), StateAction, Name, Parent, Cost) | RestPairs]) :-
    transform_to_key_value_euristic(Rest, RestPairs).

sort_by_euristic(List, SortedList) :-
    transform_to_key_value_euristic(List, KeyValuePairs),
    keysort(KeyValuePairs, SortedKeyValuePairs),
    extract_state_values(SortedKeyValuePairs, SortedList).

extract_state_values([], []).

extract_state_values([_-State | RestPairs], [State | RestValues]) :-
    extract_state_values(RestPairs, RestValues).
