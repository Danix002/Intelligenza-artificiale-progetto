/**
 * Mosse applicabili
**/
:-[knowledge_example].

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