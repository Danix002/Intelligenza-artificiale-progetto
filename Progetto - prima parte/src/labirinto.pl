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

has_hammer(0).

applicable(nord, pos(_, R,C)) :-
      pos(monster_position, R, C),
      R > 1,
      R1 is R - 1,
      pos(empty, R1, C).

applicable(nord, pos(_, R,C)) :-
    pos(monster_position, R, C),
    R > 1,
    R1 is R - 1,
    pos(destroyable_wall, R1, C),
    has_hammer(N),
    N > 0.

applicable(nord, pos(_, R,C)) :-
    pos(gem, R, C), 
    R > 1,
    R1 is R-1,
    pos(empty, R1, C).
    
/**applicable(sud, pos(_, R,C)) :-
    pos(monster_position, R, C),
    R<7,
    R1 is R+1,
    pos(empty, R1, C).

applicable(sud, pos(_, R,C)) :-
    pos(monster_position, R, C),
    R<7,
    R1 is R+1,
    pos(destroyable_wall, R1, C),
    has_hammer(N),
    N > 0.

applicable(sud, pos(_, R,C)) :-
    pos(gem, R, C),
    R<7,
    R1 is R+1,
    pos(empty, R1, C).

applicable(est, pos(_, R,C)) :-
    pos(monster_position, R, C),
    C<7,
    C1 is C+1,
    pos(empty, R1, C).

applicable(est, pos(_, R,C)) :-
    pos(monster_position, R, C),
    C<7,
    C1 is C+1,
    pos(destroyable_wall, R, C1),
    has_hammer(N),
    N > 0.

applicable(est, pos(_, R,C)) :-
    pos(gem, R, C),
    C<7,
    C1 is C+1,
    pos(empty, R1, C).    

applicable(ovest, pos(_, R,C)) :-
    pos(monster_position, R, C),
    C>1,
    C1 is C-1,
    pos(empty, R1, C).

applicable(ovest, pos(_, R,C)) :-
    pos(monster_position, R, C),
    C>1,
    C1 is C-1,
    pos(destroyable_wall,R, C1),
    has_hammer(N),
    N > 0.

applicable(ovest, pos(_, R,C)) :-
    pos(gem, R, C),
    C>1,
    C1 is C-1,
    pos(empty, R1, C).

applicable(pickup, pos(_, R,C)) :-
    pos(hammer, R, C),
    \+ has_hammer(N),
    N1 is N+1.*/

ricerca(Cammino) :-
    pos(monster_position, R, C),
    findall(pos(gem, RG, CG), pos(gem, RG, CG), Lpos),
    profondity_search([pos(monster_position, R, C)|Lpos], Cammino),
    print(Cammino).

profondity_search(R, C, []) :- pos(portal, R, C), !.

profondity_search(S, [Az|SeqAzioni]) :-
    findall(S, applicable(Az, S), Lpos),
    print(Lpos),
    trasform(Az, Lpos, L1pos),
    print(L1pos),
    profondity_search(L1pos, SeqAzioni).

trasform(nord, [pos(_, R, C)| Tail], L1pos) :- 
    det_position_nord(pos(_, R, C), R1),
    trasform(nord, Tail, [pos(_, R1, C)|L1pos]).

trasform(nord, [], L1pos).

/**trasform(sud, [pos(_, R, C)| Tail], L1pos) :- 
    det_position_sud(pos(_, R, C), R1),
    trasform(sud, Tail, [pos(_, R1, C)|L1pos]).

trasform(sud, [], L1pos).

trasform(ovest, [pos(_, R, C)| Tail], L1pos) :- 
    det_position_ovest(pos(_, R, C), C1),
    trasform(ovest, Tail, [pos(_, R, C1)|L1pos]).

trasform(ovest, [], L1pos).

trasform(est, [pos(_, R, C)| Tail], L1pos) :- 
    det_position_est(pos(_, R, C), C1),
    trasform(est, Tail, [pos(_, R, C1)|L1pos]).

trasform(est, [], L1pos).*/

det_position_nord(pos(_, R, C), R1) :- 
    R > 1,
    RTMP is R - 1,
    pos(wall, RTMP, C),
    R1 is R.

det_position_nord(pos(_, R, C), R1) :- 
    R > 1,
    RTMP is R - 1,
    pos(empty, RTMP, C),
    det_position_nord(pos(_, RTMP, C), R1).

det_position_nord(pos(_, R, C), R1) :- 
    R = 1,
    R1 is R.

/**det_position_sud(pos(_, R, C), R1) :-
    R < 7,
    RTMP is R - 1,
    pos(wall, RTMP, C),
    R1 is R.

det_position_sud(pos(_, R, C), R1) :-
    R < 7,
    R1 is R+1,
    pos(empty, RTMP, C),
    det_position_sud(pos(_, RTMP, C), R1).

det_position_sud(pos(_, R, C), R1) :-
    R = 7,
    R1 is R.

det_position_ovest(pos(_, R, C), C1) :-
    C > 1,
    CTMP is C - 1,
    pos(wall, R, CTMP),
    C1 is C.

det_position_ovest(pos(_, R, C), C1) :-
    C > 1,
    C1 is C - 1,
    pos(empty, R, CTMP),
    det_position_ovest(pos(_, R, CTMP), C1).

det_position_ovest(pos(_, R, C), C1) :-
    C = 1,
    C1 is C.

det_position_est(pos(_, R, C), C1) :-
    C < 7,
    CTMP is C - 1,
    pos(wall, R, CTMP),
    C1 is C.

det_position_est(pos(_, R, C), C1) :-
    C < 7,
    C1 is C+1,
    pos(empty, R, CTMP),
    det_position_est(pos(_, R, CTMP), C1).

det_position_est(pos(_, R, C), C1) :-
    C = 7,
    C1 is C.



*/