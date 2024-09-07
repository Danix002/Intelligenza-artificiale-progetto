% Definizione del labirinto little size more walls
pos(wall, 0, 0).
pos(empty, 0, 1).
pos(empty, 0, 2).
pos(empty, 0, 3).
pos(empty, 0, 4).
pos(empty, 0, 5).
pos(portal, 0, 6).

pos(empty, 1, 0).
pos(empty, 1, 1).
pos(empty, 1, 2).
pos(empty, 1, 3).
pos(wall, 1, 4).
pos(empty, 1, 5).
pos(empty, 1, 6).

pos(empty, 2, 0).
pos(empty, 2, 1).
pos(empty, 2, 2).
pos(empty, 2, 3).
pos(wall, 2, 4).
pos(empty, 2, 5).
pos(empty, 2, 6).

pos(empty, 3, 0).
pos(empty, 3, 1).
pos(empty, 3, 2).
pos(empty, 3, 3).
pos(empty, 3, 4).
pos(empty, 3, 5).
pos(empty, 3, 6).

pos(monster_position, 4, 0).
pos(empty, 4, 1).
pos(empty, 4, 2).
pos(wall, 4, 3).
pos(empty, 4, 4).
pos(empty, 4, 5).
pos(empty, 4, 6).

pos(empty, 5, 0).
pos(empty, 5, 1).
pos(empty, 5, 2).
pos(empty, 5, 3).
pos(wall, 5, 4).
pos(wall, 5, 5).
pos(portal, 5, 6).

azione(nord).
azione(sud).
azione(est).
azione(ovest).

has_hammer(0).