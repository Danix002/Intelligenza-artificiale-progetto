/**
 * Determinazione dello stato successivo
**/

:-[det_position].

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