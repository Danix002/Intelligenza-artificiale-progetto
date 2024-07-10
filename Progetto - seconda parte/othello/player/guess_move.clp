(deftemplate move
   (slot step)
   (slot row)
   (slot col)
   (slot cost)
)

(deftemplate time
  (slot step)
)

(deftemplate game-difficulty
  (slot difficulty (allowed-values easy hard vhard))
)

(deftemplate cell-direction
    (slot row)
    (slot col)
)

(deffacts initial-time
   (time (step -1))
)

(deftemplate counted-cell
  (slot step)
  (slot row)
  (slot col)
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

(deftemplate white-border-counter
  (slot step)
  (slot position) 
  (slot index) 
  (slot count)
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
  (not (exists (white-border-counter (step ?s) (position row) (index 0))))
  (not (exists (white-border-counter (step ?s) (position row) (index 7))))
  (not (exists (white-border-counter (step ?s) (position col) (index 0))))
  (not (exists (white-border-counter (step ?s) (position col) (index 7))))
=>
  (assert (white-border-counter (step ?s) (position row) (index 0) (count 0)))
  (assert (white-border-counter (step ?s) (position row) (index 7) (count 0)))
  (assert (white-border-counter (step ?s) (position col) (index 0) (count 0)))
  (assert (white-border-counter (step ?s) (position col) (index 7) (count 0)))
  (printout t "Initialized white border counters for step " ?s crlf)
)

(defrule initialize-selected-cell-frontier-counter-bottom
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (< ?r1 ?r2) (eq ?c1 ?c2)))
  ?direction <- (cell-direction(row 1)  (col 0))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell)(direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter bottom " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-top
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (> ?r1 ?r2) (eq ?c1 ?c2)))
  ?direction <- (cell-direction(row -1)  (col 0))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter top " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col)crlf)
)

(defrule initialize-selected-cell-frontier-counter-left
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (> ?c1 ?c2) (eq ?r1 ?r2)))
  ?direction <-(cell-direction (row 0) (col -1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter left " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-right
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (< ?c1 ?c2) (eq ?r1 ?r2)))
  ?direction <- (cell-direction (row 0) (col 1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter right " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance  " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-first-left
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (> ?c1 ?c2) (> ?r1 ?r2)))
  (test (eq (- ?c2 ?c1) (- ?r2 ?r1)))
  ?direction <- (cell-direction (row -1) (col -1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter diagonal first left " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance  " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-first-right
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (< ?c1 ?c2) (< ?r1 ?r2)))
  (test (eq (- ?c2 ?c1) (- ?r2 ?r1)))
  ?direction <- (cell-direction (row 1) (col 1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter diagonal first right " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-second-left
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (< ?c1 ?c2) (> ?r1 ?r2)))
  (test (eq (+ ?c2 ?c1) (+ ?r2 ?r1)))
  ?direction <- (cell-direction (row -1) (col 1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter diagonal second left " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance  " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule initialize-selected-cell-frontier-counter-diagonal-second-right
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?destination_cell <- (cell (step ?s) (row ?r2) (col ?c2) (content white))
  ?start_cell <- (cell (step ?s) (row ?r1) (col ?c1) (content empty))
  (test (and (> ?c1 ?c2) (< ?r1 ?r2)))
  (test (eq (+ ?c2 ?c1) (+ ?r2 ?r1)))
  ?direction <- (cell-direction (row 1) (col -1))
  (not (exists (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (direction ?direction))))
=>
  (bind ?distance (sqrt (+ (** (- ?r2 ?r1) 2) (** (- ?c2 ?c1) 2))))
  (assert (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier 0) (direction ?direction) (distance ?distance)))
  (printout t "Initialized selected cell frontier counter diagonal second right " ?s " start cell row " ?r1 " col " ?c1 " destination cell row " ?r2 " col " ?c2 " distance " ?distance " direction: row " (fact-slot-value ?direction row) 
            " col " (fact-slot-value ?direction col) crlf)
)

(defrule increment-cells-selected-frontier-counter
  (declare (salience 10))
  ?t <- (time (step ?s))
  ?scfc <- (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier ?counter) (direction ?direction) (distance ?distance))
  (test (> ?distance 0))
  ?cell <- (cell (step ?s) (row ?r) (col ?c) (content white) (type ?type))
  (test (and (eq r (+ (fact-slot-value ?direction row) (* (fact-slot-value ?direction row) ?distance))) 
             (eq c (+ (fact-slot-value ?direction col) (* (fact-slot-value ?direction col) ?distance)))
             (eq ?type F)
  ))
=>
  (bind ?counter (+ ?counter 1))
  (bind ?distance (- ?distance 1))
  (modify ?scfc (count_frontier ?counter) (distance ?distance))
  (printout t "increment-cells-selected-frontier-counter distance " ?distance " counter " ?counter crlf)
)

(defrule increment-cells-selected-no-frontier-counter
  (declare (salience 10))
  ?t <- (time (step ?s))
  ?scfc <- (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier ?counter) (direction ?direction) (distance ?distance))
  (test (> ?distance 0))
  ?cell <- (cell (step ?s) (row ?r) (col ?c) (content white) (type ?type))
  (test (and (eq r (+ (fact-slot-value ?direction row) (* (fact-slot-value ?direction row) ?distance))) 
             (eq c (+ (fact-slot-value ?direction col) (* (fact-slot-value ?direction col) ?distance)))
             (not(eq ?type F))
  ))
=>
  (bind ?distance (- ?distance 1))
  (modify ?scfc (distance ?distance))
  (printout t "increment-cells-selected-no-frontier-counter distance " ?distance " counter " ?counter crlf)
)

(defrule count-white-border-cells-row-0 
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?c <- (cell (step ?s) (row 0) (col ?col) (content white))
  (not (counted-cell (step ?s) (row 0) (col ?col)))
  ?b <- (white-border-counter (step ?s) (position row) (index 0) (count ?cnt))
  (test (not (or (eq ?col 0) (eq ?col 7)))) 
=>
  (modify ?b (count (+ ?cnt 1)))
  (assert (counted-cell (step ?s) (row 0) (col ?col)))
  (printout t "step " ?s " Counted white cell at row 0 , col" ?col", new count: " (+ ?cnt 1) crlf)
)

(defrule count-white-border-cells-row-7 
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?c <- (cell (step ?s) (row 7) (col ?col) (content white))
  (not (counted-cell (step ?s) (row 7) (col ?col)))
  ?b <- (white-border-counter (step ?s) (position row) (index 7) (count ?cnt))
  (test (not (or (eq ?col 0) (eq ?col 7)))) 
=>
  (modify ?b (count (+ ?cnt 1)))
  (assert (counted-cell (step ?s) (row 7) (col ?col)))
  (printout t "step " ?s " Counted white cell at row  row 7 , col "?col", new count: " (+ ?cnt 1) crlf)
)

(defrule count-white-border-cells-col-0 
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?c <- (cell (step ?s) (row ?row) (col 0) (content white))
  (not (counted-cell (step ?s) (row ?row) (col 0)))
  ?b <- (white-border-counter (step ?s) (position col) (index 0) (count ?cnt))
  (test (not (or (eq ?row 0) (eq ?row 7)))) 
=>
  (modify ?b (count (+ ?cnt 1)))
  (assert (counted-cell (step ?s) (row ?row) (col 0)))
  (printout t "step " ?s " Counted white cell at row " ?row ", col 0, new count: " (+ ?cnt 1) crlf)
)

(defrule count-white-border-cells-col-7 
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?c <- (cell (step ?s) (row ?row) (col 7) (content white))
  (not (counted-cell (step ?s) (row ?row) (col 7)))
  ?b <- (white-border-counter (step ?s) (position col) (index 7) (count ?cnt))
  (test (not (or (eq ?row 0) (eq ?row 7)))) 
=>
  (modify ?b (count (+ ?cnt 1)))
  (assert (counted-cell (step ?s) (row ?row) (col 7)))
  (printout t "step " ?s "Counted white cell at row " ?row ", col 7, new count: " (+ ?cnt 1) crlf)
)


(defrule count-white-border-cells-corner
  (declare (salience 11))
  ?t <- (time (step ?s))
  ?c <- (cell (step ?s) (row ?row) (col ?col) (content white) (type COR))
  ?b <- (white-border-counter (step ?s) (position row) (index ?row) (count ?rcnt))
  ?r <- (white-border-counter (step ?s) (position col) (index ?col) (count ?ccnt))
  (not (counted-cell (step ?s) (row ?row) (col ?col)))
=>
  (modify ?b (count (+ ?rcnt 1)))
  (modify ?r (count (+ ?ccnt 1)))
  (assert (counted-cell (step ?s) (row ?row) (col ?col)))
  (printout t "step " ?s "Counted white CORNER cell at row " ?row ", col " ?col ", new row count: "  (+ ?rcnt 1) ", new col count: " (+ ?ccnt 1)  crlf)
)

(defrule forget-past-moves  
  (time (step ?s)) 
  ?m <- (move (step ?s3&:(< ?s3 ?s)) (row ?r) (col ?c))
=>
  (retract ?m)
)

(defrule forget-past-counters
  (declare (salience 12))
  ?t <- (time (step ?s))
  ?c <- (white-border-counter (step ?s3&:(< ?s3 ?s)) (position ?pos) (index ?idx) (count ?cnt))
=>
  (retract ?c)
  (printout t "Retracted past counter at step " ?s3 crlf)
)

(defrule set-cell-type-X (declare (salience 12))
  ?t <- (time (step ?s))
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a)  (type empty))
  (test (or (and (eq ?r 1) (eq ?c 1)) (and (eq ?r 6) (eq ?c 6)) (and (eq ?r 6) (eq ?c 1)) (and (eq ?r 1) (eq ?c 6))))
=>
  (modify ?cl (type X))
)

(defrule set-cell-type-C (declare(salience 12))
  ?t <- (time (step ?s))
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
    ?cl <- (cell (step ?s) (row ?r) (col ?c) (content ?cont) (type empty))
    (test (not (eq ?cont empty)))
    ?dir <- (cell-direction (row ?rdir) (col ?cdir))
    ?c1dir <- (cell (step ?s) (row ?nr) (col ?nc) (content empty))
    (test (and (eq ?nr (+ ?r ?rdir)) (eq ?nc (+ ?c ?cdir))))
  =>
    (modify ?cl (type F))
    (printout t "c1dir: row " ?nr ", col " ?nc crlf)
    (bind ?nr (+ ?r ?rdir))
    (bind ?nc (+ ?c ?cdir))
    (printout t "nrow " ?nr " nrcol " ?nc crlf)
    (printout t "step " ?s " cell modified: row " ?r ", col " ?c ", because direction row " ?rdir ", col " ?cdir crlf)
)



(defrule set-cell-type-cor (declare (salience 12))
  ?t <- (time (step ?s))
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a)  (type empty))
  (test (or (and (eq ?r 0) (eq ?c 7)) (and (eq ?r 7) (eq ?c 0)) (and (eq ?r 0) (eq ?c 0)) (and (eq ?r 7) (eq ?c 7))))
=>
  (modify ?cl (type COR))
)


(defrule update-cost-of-cell (declare (salience 9))
  ?t <- (time (step ?s))
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty) (type empty))
=>
  (bind ?dist-top-left (sqrt (+ (** (- 0 ?r) 2) (** (- 0 ?c) 2))))
  (bind ?dist-top-right (sqrt (+ (** (- 0 ?r) 2) (** (- 7 ?c) 2))))
  (bind ?dist-bottom-left (sqrt (+ (** (- 7 ?r) 2) (** (- 0 ?c) 2))))
  (bind ?dist-bottom-right (sqrt (+ (** (- 7 ?r) 2) (** (- 7 ?c) 2))))
  (bind ?new-cost (min ?dist-top-left ?dist-top-right ?dist-bottom-left ?dist-bottom-right))
  (modify ?cl (nearCorner ?new-cost))
)

(defrule update-cost-of-certain-cell-X (declare (salience 8))
  ?t <- (time (step ?s))
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty)  (type X))
=>
  (bind ?new-cost 20)
  (modify ?cl (nearCorner ?new-cost))
)

(defrule update-cost-of-certain-cell-C (declare (salience 8))
  ?t <- (time (step ?s))
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty) (type C))
  ?b <- (white-border-counter (step ?s) (position ?p ) (index ?i) (count ?cnt)) 
  (test (or (and (eq ?p row) (eq ?i ?r))
            (and (eq ?p col) (eq ?i ?c))))
  (test (< ?cnt 3))
=> 
  (bind ?new-cost 20)
  (modify ?cl (nearCorner ?new-cost))
  (printout t "modify cell c " ?r "col " ?c "with counter " ?cnt " and r " ?r crlf)
)


(defrule update-cell-selected-cost
  (declare (salience 7))
  ?t <- (time (step ?s))
  ?scfc <- (selected-cell-frontier-counter (step ?s) (start_cell ?start_cell) (destination_cell ?destination_cell) (count_frontier ?counter) (direction ?direction) (distance ?distance))
  (test (<= ?distance 0))
  ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a))
  (test (and (eq ?r (fact-slot-value ?start_cell row)) (eq ?c (fact-slot-value ?start_cell col))))
=>
  (bind ?new-cost (+ ?a ?counter))
  (modify ?cl (nearCorner ?new-cost))
  (printout t "update-cell-selected-cost: modify cell c " ?r "col " ?c "with counter " ?counter" and r " ?r crlf)
)

(defrule guess-move 
  ?t <- (time (step ?s))
  (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty))
=>
  (bind ?s3 (+ ?s 1))
  (assert (move (step ?s3) (row ?r) (col ?c) (cost ?a)))
  (modify ?t (step ?s))
)

(defrule str 
  (declare (salience 10))
=>
  (set-strategy depth)
)
