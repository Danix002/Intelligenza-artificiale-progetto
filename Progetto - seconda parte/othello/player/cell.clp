;(defmodule CELL ( export ?ALL ) (import CONTROL deftemplate ?ALL) )

(deftemplate cell-direction
    (slot row)
    (slot col)
)

(deftemplate counted-cell
  (slot step)
  (slot row)
  (slot col)
)

(deftemplate white-border-counter
  (slot step)
  (slot position) 
  (slot index) 
  (slot count)
)

(deftemplate selected-cell-frontier-counter
  (slot step)
  (multislot start_cell)
  (multislot destination_cell)
  (slot count_frontier)
  (multislot direction)
  (slot distance)
)

(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot nearCorner)
  (slot content (allowed-values empty white black))
  (slot type (allowed-values empty COR C A B X F))
)

(deffacts cell-directions
  (cell-direction (row -1) (col -1))
  (cell-direction (row -1) (col 0))
  (cell-direction (row -1) (col +1))
  (cell-direction (row  0) (col -1))
  (cell-direction (row  0) (col +1))
  (cell-direction (row +1) (col -1))
  (cell-direction (row +1) (col 0))
  (cell-direction (row +1) (col +1))
)

(deffacts initialize-white-border-counters
  (white-border-counter (step -1) (position row) (index 0) (count 0))
  (white-border-counter (step -1) (position row) (index 7) (count 0))
  (white-border-counter (step -1) (position col) (index 0) (count 0))
  (white-border-counter (step -1) (position col) (index 7) (count 0))
)

(defrule initialize-white-border-counters 
  (declare (salience 12))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(not (eq ?d easy))) )
  (not (exists (white-border-counter (step ?s) (position row) (index 0))))
  (not (exists (white-border-counter (step ?s) (position row) (index 7))))
  (not (exists (white-border-counter (step ?s) (position col) (index 0))))
  (not (exists (white-border-counter (step ?s) (position col) (index 7))))
=>
  (assert (white-border-counter (step ?s) (position row) (index 0) (count 0)))
  (assert (white-border-counter (step ?s) (position row) (index 7) (count 0)))
  (assert (white-border-counter (step ?s) (position col) (index 0) (count 0)))
  (assert (white-border-counter (step ?s) (position col) (index 7) (count 0)))
  ;(printout t "Initialized white border counters for step " ?s crlf)
)

(defrule initialize-selected-cell-frontier-counter-bottom
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(< ?r1 ?r2)) (col ?c2&:(eq ?c1 ?c2)) (content white))
  ?direction <- (cell-direction(row 1)  (col 0))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell)(direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter bottom " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-top
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(> ?r1 ?r2)) (col ?c2&:(eq ?c1 ?c2)) (content white))
  ?direction <- (cell-direction(row -1)  (col 0))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter top " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col)crlf)
)

(defrule initialize-selected-cell-frontier-counter-left
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(eq ?r1 ?r2)) (col ?c2&:(> ?c1 ?c2)) (content white))
  ?direction <- (cell-direction (row 0) (col -1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter left " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-right
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(eq ?r1 ?r2)) (col ?c2&:(< ?c1 ?c2)) (content white))
  ?direction <- (cell-direction (row 0) (col 1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter right " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance  " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-first-left
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(> ?r1 ?r2)) (col ?c2&:(> ?c1 ?c2) &:(eq (- ?r1 ?c1) (- ?r2 ?c2))) (content white))
  ?direction <- (cell-direction (row -1) (col -1))
  
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter diagonal first left " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance  " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-first-right
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(< ?r1 ?r2)) (col ?c2&:(< ?c1 ?c2) &:(eq (- ?r1 ?c1) (- ?r2 ?c2))) (content white))
  ?direction <- (cell-direction (row 1) (col 1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter diagonal first right " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-second-left
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(> ?r1 ?r2)) (col ?c2&:(< ?c1 ?c2) &:(eq (+ ?r1 ?c1) (+ ?r2 ?c2))) (content white))
  ?direction <- (cell-direction (row -1) (col 1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter diagonal second left " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance  " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-second-right
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  ?destination_cell <- (cell (step ?s) (row ?r2&:(< ?r1 ?r2)) (col ?c2&:(> ?c1 ?c2) &:(eq (+ ?r1 ?c1) (+ ?r2 ?c2))) (content white))
  ?direction <- (cell-direction (row 1) (col -1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (-  (max (abs (- ?r2 ?r1)) (abs (- ?c2 ?c1) ) ) 1)  )
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  ;(printout t "Initialized selected cell frontier counter diagonal second right " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            ;" col " (fact-slot-value ?direction col) crlf)
)

(defrule set-cell-type-X (declare (salience 12))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(not (eq ?d easy))) )
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a)  (type empty))
  (test (or (and (eq ?r 1) (eq ?c 1)) (and (eq ?r 6) (eq ?c 6)) (and (eq ?r 6) (eq ?c 1)) (and (eq ?r 1) (eq ?c 6))))
=>
  (modify ?cl (type X))
)

(defrule set-cell-type-C (declare(salience 12))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(not (eq ?d easy))) )
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (type empty))
  (test (or (and (eq ?r 1) (eq ?c 0)) 
             (and (eq ?r 0) (eq ?c 1)) 
             (and (eq ?r 0) (eq ?c 6)) 
             (and (eq ?r 1) (eq ?c 7)) 
             (and (eq ?r 6) (eq ?c 0)) 
             (and (eq ?r 7) (eq ?c 1)) 
             (and (eq ?r 7) (eq ?c 6))
             (and (eq ?r 6) (eq ?c 7))))
=>
  (modify ?cl (type C))
)

(defrule set-cell-type-B (declare(salience 12))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(not (eq ?d easy))) )
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (type empty))
  (test (or (and (eq ?r 3) (eq ?c 0)) 
             (and (eq ?r 4) (eq ?c 0)) 
             (and (eq ?r 0) (eq ?c 3)) 
             (and (eq ?r 0) (eq ?c 4)) 
             (and (eq ?r 3) (eq ?c 7)) 
             (and (eq ?r 4) (eq ?c 7)) 
             (and (eq ?r 7) (eq ?c 3)) 
             (and (eq ?r 7) (eq ?c 4))))
=>
  (modify ?cl (type B))
)

(defrule set-cell-type-A (declare(salience 12))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(not (eq ?d easy))) )
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (type empty))
  (test (or (and (eq ?r 0) (eq ?c 2)) 
             (and (eq ?r 0) (eq ?c 5)) 
             (and (eq ?r 2) (eq ?c 0)) 
             (and (eq ?r 5) (eq ?c 0)) 
             (and (eq ?r 7) (eq ?c 2)) 
             (and (eq ?r 7) (eq ?c 5)) 
             (and (eq ?r 2) (eq ?c 7)) 
             (and (eq ?r 5) (eq ?c 7))))
=>
  (modify ?cl (type A))
)

(defrule set-cell-type-F (declare (salience 11))
    ?t <- (time (step ?s))
    ?difficulty <- (game-difficulty (difficulty ?d&:(eq ?d vhard)) )
    ?cl <- (cell (step ?s) (row ?r) (col ?c) (content ?cont) (type empty))
    (test (not (eq ?cont empty)))
    ?dir <- (cell-direction (row ?rdir) (col ?cdir))
    ?c1dir <- (cell (step ?s) (row ?nr) (col ?nc) (content empty))
    (test (and (eq ?nr (+ ?r ?rdir)) (eq ?nc (+ ?c ?cdir))))
  =>
    (modify ?cl (type F))
    ;(printout t "c1dir: row " ?nr ", col " ?nc crlf)
    (bind ?nr (+ ?r ?rdir))
    (bind ?nc (+ ?c ?cdir))
    ;(printout t "nrow " ?nr " nrcol " ?nc crlf)
    ;(printout t "step " ?s " cell modified: row " ?r ", col " ?c ", because direction row " ?rdir ", col " ?cdir crlf)
)

(defrule set-cell-type-cor (declare (salience 12))
  ?t <- (time (step ?s))
  ?difficulty <- (game-difficulty (difficulty ?d&:(not (eq ?d easy))) )
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a)  (type empty))
  (test (or (and (eq ?r 0) (eq ?c 7)) (and (eq ?r 7) (eq ?c 0)) (and (eq ?r 0) (eq ?c 0)) (and (eq ?r 7) (eq ?c 7))))
=>
  (modify ?cl (type COR))
)





