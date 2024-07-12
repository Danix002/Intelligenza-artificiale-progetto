wall(pos(0,0)).
hammer(pos(0,1)).
empty(pos(0,2)).
empty(pos(0,3)).
empty(pos(0,4)).
empty(pos(0,5)).
empty(pos(0,6)).
gem(pos(0,7)).

empty(pos(1,0)).
empty(pos(1,1)).
empty(pos(1,2)).
empty(pos(1,3)).
wall(pos(1,4)).
empty(pos(1,5)).
empty(pos(1,6)).
empty(pos(1,7)).

empty(pos(2,0)).
portal(pos(2,1)).
empty(pos(2,2)).
empty(pos(2,3)).
empty(pos(2,4)).
empty(pos(2,5)).
empty(pos(2,6)).
empty(pos(2,7)).

empty(pos(3,0)).
empty(pos(3,1)).
empty(pos(3,2)).
empty(pos(3,3)).
empty(pos(3,4)).
empty(pos(3,5)).
empty(pos(3,6)).
empty(pos(3,7)).

empty(pos(4,0)).
empty(pos(4,1)).
empty(pos(4,2)).
wall(pos(4,3)).
gem(pos(4,4)).
empty(pos(4,5)).
empty(pos(4,6)).
empty(pos(4,7)).

empty(pos(5,0)).
empty(pos(5,1)).
empty(pos(5,2)).
empty(pos(5,3)).
wall(pos(5,4)).
wall(pos(5,5)).
empty(pos(5,6)).
wall(pos(5,7)).

empty(pos(6,0)).
destroyable_wall(pos(6,1)).
destroyable_wall(pos(6,2)).
destroyable_wall(pos(6,3)).
wall(pos(6,4)).
empty(pos(6,5)).
empty(pos(6,6)).
monster_position(pos(6,7)).

wall(pos(7,0)).
gem(pos(7,1)).
empty(pos(7,2)).
empty(pos(7,3)).
wall(pos(7,4)).
wall(pos(7,5)).
empty(pos(7,6)).
empty(pos(7,7)).
has_hammer(0).


applicable(nord,pos(R,C)) :-
      monster_position(pos(R, C)),
      R>1,
      R1 is R-1,
      not(wall(pos(R1, C))).

applicable(nord,pos(R,C)) :-
    monster_position(pos(R,C)),
    R>1,
    R1 is R-1,
    destroyable_wall(pos(R1, C)),
    has_hammer(N),
    N > 0.


applicable(nord,pos(R,C)) :-
    gem(pos(R, C)), 
    R>1,
    R1 is R-1,
    not(wall(pos(R1, C))),
    not(destroyable_wall(pos(R1, C))).
    



ricerca(Cammino) :-
    monster_position(S0),
    findall(S, gem(S), Lpos),
    profondity_search([monster_position|Lpos], Cammino).

profondity_search(S,[]) :- portal(S), !.

profondity_search(S, [Az|SeqAzioni]) :-
    findall(P, applicable(nord, P), Lpos),
    print(search),
    print(Lpos),
    trasform(Az, Lpos, L1pos),
    print(L1pos).
    %profondity_search(S1, SeqAzioni).


trasform(nord, [ pos(R, C)| Tail ], L1pos) :- 
    det_position(pos(R, C), R1),
    trasform(nord, Tail, [pos(R1,C)|L1pos]).


det_position(pos(R, C), R1) :- 
    R>1,
    R1 is R-1,
    wall(pos(R1, C)).

det_position(pos(R, C), R1) :- 
    R>1,
    R1 is R-1,
    not(wall(pos(R1, C))),
    det_position(pos(R1, C), R1).



det_position(pos(R, C), R1) :- 
    R=1.

trasform(nord, [], L1pos).
