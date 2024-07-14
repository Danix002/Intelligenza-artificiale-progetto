:- discontiguous pos/3.
:- discontiguous has_hammer/1.
:- discontiguous trasform/3.

% Definizione del labirinto
pos(wall, 0, 0).
pos(hammer, 0, 1).
pos(empty, 0, 2).
pos(empty, 0, 3).
pos(empty, 0, 4).
pos(empty, 0, 5).
pos(empty, 0, 6).
pos(gem, 0, 7).

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

applicable(nord, pos(monster_position, R,C), [ _ | GemState]) :-
    R > 0,
    R1 is R - 1,
    \+ member( pos(gem, R1, C) , GemState ),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C).
    % TODO DESCRUTIBLE WALL

/**
applicable(nord, pos(monster_position, R,C), [ MonsterState | GemState]) :-
    R > 0,
    R1 is R - 1,
    pos(destroyable_wall, R1, C),
    has_hammer(N),
    N > 0.
    **/

applicable(nord, pos(gem, R,C), [ MonsterState | GemState]) :-
    R > 0,
    R1 is R-1,
    \+ member( pos(gem, R1, C) , GemState ),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C).

applicable(sud, pos(monster_position, R,C), [ _ | GemState]) :-
    R < 7,
    R1 is R + 1,
    \+ member( pos(gem, R1, C) , GemState ),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C).
/**
applicable(sud, pos(monster_position, R,C), [ MonsterState | GemState]) :-
    R < 7,
    R1 is R + 1,
    pos(destroyable_wall, R1, C),
    has_hammer(N),
    N > 0.
**/

applicable(sud, pos(gem, R,C), [ MonsterState | GemState]) :-
    R < 7,
    R1 is R + 1,
    \+ member( pos(gem, R1, C) , GemState ),
    MonsterState \= pos(monster_position, R1, C),
    \+ pos(wall, R1, C),
    \+ pos(destroyable_wall, R1, C).

applicable(est, pos(monster_position, R,C), [ _ | GemState]) :-
    C < 7,
    C1 is C + 1,
    \+ member( pos(gem, R, C1) , GemState ),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).

/**
applicable(est, pos(monster_position, R,C), [ MonsterState | GemState]) :-
    C < 7,
    C1 is C + 1,
    pos(destroyable_wall, R, C1),
    has_hammer(N),
    N > 0.
**/
applicable(est, pos(gem, R,C), [ MonsterState | GemState]) :-
    C < 7,
    C1 is C + 1,
    \+ member( pos(gem, R, C1) , GemState ),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).   

applicable(ovest, pos(monster_position, R,C), [ _ | GemState]) :-
    C > 0,
    C1 is C - 1,
    \+ member( pos(gem, R, C1) , GemState ),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).
/**
applicable(ovest, pos(monster_position, R,C), [ MonsterState | GemState]) :-
    C > 0,
    C1 is C - 1,
    pos(destroyable_wall, R, C1),
    has_hammer(N),
    N > 0.
**/
applicable(ovest, pos(gem, R,C), [ MonsterState | GemState]) :-
    C > 0,
    C1 is C - 1,
    \+ member( pos(gem, R, C1) , GemState ),
    MonsterState \= pos(monster_position, R, C1),
    \+ pos(wall, R, C1),
    \+ pos(destroyable_wall, R, C1).

/**
applicable(pickup, pos(_, R,C)) :-
    pos(hammer, R, C),
    has_hammer(N),
    N is N + 1.
*/

ricerca(Cammino):-
    pos(monster_position, R, C),
    findall(pos(gem, RG, CG), pos(gem, RG, CG), Lpos),
    profondity_search(pos(monster_position, R, C), Lpos, Cammino).
    %print(Cammino).

profondity_search(pos(monster_position, MonsterRow, MonsterCol), _, []) :- pos(portal, MonsterRow, MonsterCol), !.

profondity_search(pos(monster_position, MonsterRow, MonsterCol), GemState, [Az|SeqAzioni]) :-
    applicable(
        Az, 
        pos(monster_position, MonsterRow, MonsterCol),
        [ pos(monster_position, MonsterRow, MonsterCol) | GemState]
    ),
    print(Az),
    nl,
    initTransform(Az, [pos(monster_position, MonsterRow, MonsterCol) | GemState ], [ TransformedPositionMonster | TransformedPositionGem]),    
    profondity_search(TransformedPositionMonster, TransformedPositionGem, SeqAzioni).




/**


findall_applicable_action([pos(T, R, C)| Tail], Result, L1pos, Az):-
    applicable(Az, pos(T, R, C)),
    findall_applicable_action(Tail, Result, [pos(T, R, C) | L1pos] , Az).

findall_applicable_action([], Result, L1pos, _):-
    L1pos \== [],
    Result = L1pos .


findall_applicable_action([pos(T, R, C)| Tail], Result, L1pos, Az):-
    \+ applicable(Az, pos(T, R, C)),
    findall_applicable_action(Tail, Result, L1pos, Az).
**/
initTransform(Az, State, Result) :- transform(Az, State, Result, State).

transform(nord, [pos(T, R, C)| Tail], [ HP | TP], State) :- 
    det_position_nord(pos(T, R, C), R1, State),
    HP = pos(T, R1, C),
    transform(nord, Tail, TP, State).

transform(nord, [], [], _) :- true.

transform(sud, [pos(T, R, C)| Tail], [ HP | TP], State) :- 
    det_position_sud(pos(T, R, C), R1, State),
    HP = pos(T, R1, C),
    transform(sud, Tail, TP, State).

transform(sud, [], [], _):- true.

transform(ovest, [pos(T, R, C)| Tail], [ HP | TP], State) :- 
    det_position_ovest(pos(T, R, C), C1, State),
    HP = pos(T, R, C1),
    transform(ovest, Tail, TP, State).

transform(ovest, [], [], _) :- true.

transform(est, [pos(T, R, C)| Tail], [HP | TP], State) :- 
    det_position_est(pos(T, R, C), C1, State),
    HP = pos(T, R, C1),
    transform(est, Tail, TP, State).

transform(est, [], [], _) :- true.

/**
det_position_nord(pos(monster_position, R, C), R1) :- 
    R > 0,
    RTMP is R - 1,
    pos(destroyable_wall, RTMP, C),
    has_hammer(N),
    N > 0,
    det_position_nord(pos(monster_position, RTMP, C), R1).
**/

det_position_nord(pos(T, R, C), R1, State) :- 
    \+ applicable(nord, pos(T, R,C), State),
    R1 is R.

det_position_nord(pos(T, R, C), R1, State) :- 
    applicable(nord, pos(T, R,C), State),
    RTMP is R - 1,
    det_position_nord(pos(T, RTMP, C), R1, State).

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
**/ 


/**
det_position_sud(pos(monster_position, R, C), R1) :- 
    R < 7,
    RTMP is R + 1,
    pos(destroyable_wall, RTMP, C),
    has_hammer(N),
    N > 0,
    det_position_sud(pos(monster_position, RTMP, C), R1).
**/

det_position_sud(pos(T, R, C), R1, State) :-
    \+ applicable(sud, pos(T, R,C), State),
    R1 is R.

% det_position_sud(pos(gem, R, C), R1) :-
%     R < 7,
%     RTMP is R + 1,
%     pos(hammer, RTMP, C),
%     det_position_sud(pos(_, RTMP, C), R1).

% det_position_sud(pos(monster_position, R, C), R1) :-
%     R < 7,
%     RTMP is R + 1,
%     pos(hammer, RTMP, C),
%     det_position_sud(pos(_, RTMP, C), R1).

det_position_sud(pos(T, R, C), R1, State) :-
    applicable(sud, pos(T, R,C), State),
    RTMP is R + 1,
    det_position_sud(pos(_, RTMP, C), R1, State).


% det_position_ovest(pos(monster_position, R, C), C1) :- 
%     C > 0,
%     CTMP is C - 1,
%     pos(destroyable_wall, R, CTMP),
%     has_hammer(N),
%     N > 0,
%     det_position_ovest(pos(monster_position, R, CTMP), C1).

det_position_ovest(pos(T, R, C), C1, State) :-
    \+ applicable(ovest, pos(T, R,C), State),
    C1 is C.

% det_position_ovest(pos(gem, R, C), C1) :-
%     C > 0,
%     CTMP is C - 1,
%     pos(hammer, R, CTMP),
%     det_position_ovest(pos(_, R, CTMP), C1).

% det_position_ovest(pos(monster_position, R, C), C1) :-
%     C > 0,
%     CTMP is C - 1,
%     pos(hammer, R, CTMP),
%     det_position_ovest(pos(_, R, CTMP), C1).

det_position_ovest(pos(T, R, C), C1, State) :-
    applicable(ovest, pos(T, R,C), State),
    CTMP is C - 1,
    det_position_ovest(pos(_, R, CTMP), C1, State).


% det_position_est(pos(monster_position, R, C), C1) :-
%     C < 7,
%     CTMP is C + 1,
%     pos(destroyable_wall, R, CTMP),
%     has_hammer(N),
%     N > 0,
%     det_position_est(pos(monster_position, R, CTMP), C1).

det_position_est(pos(T, R, C), C1, State) :-
    \+ applicable(est, pos(T, R,C), State),
    C1 is C.

% det_position_est(pos(gem, R, C), C1) :-
%     C < 7,
%     CTMP is C + 1,
%     pos(hammer, R, CTMP),
%     det_position_est(pos(_, R, CTMP), C1).

% det_position_est(pos(monster_position, R, C), C1) :-
%     C < 7,
%     CTMP is C + 1,
%     pos(hammer, R, CTMP),
%     det_position_est(pos(_, R, CTMP), C1).

det_position_est(pos(T, R, C), C1, State) :-
    applicable(est, pos(T, R,C), State),
    CTMP is C + 1,
    det_position_est(pos(_, R, CTMP), C1, State).




