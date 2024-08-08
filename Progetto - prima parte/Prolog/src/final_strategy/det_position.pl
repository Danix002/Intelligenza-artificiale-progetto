/**
 * Determinazione della posizione finale data la mossa
**/

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