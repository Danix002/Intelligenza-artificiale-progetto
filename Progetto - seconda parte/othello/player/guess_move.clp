(deftemplate move
   (slot step)
   (slot row)
   (slot col)
   (slot cost)
)

(deftemplate time
  (slot step)
)

(deffacts initial-time
   (time (step -1))
)

(deftemplate counted-cell
  (slot step)
  (slot row)
  (slot col)
)

(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot nearCorner)
  (slot content (allowed-values empty white black))
  (slot type (allowed-values empty COR C A B X))
)

(deftemplate white-border-counter
  (slot step)
  (slot position) 
  (slot index) 
  (slot count)
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
  (printout t "step " ?s " Counted white cell at row " row ", col 0, new count: " (+ ?cnt 1) crlf)
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
  (printout t "step " ?s " Counted white cell at row " row ", col 0, new count: " (+ ?cnt 1) crlf)
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
             (and (eq ?r 6) (eq ?c 2)) 
             (and (eq ?r 6) (eq ?c 5)) 
             (and (eq ?r 2) (eq ?c 7)) 
             (and (eq ?r 5) (eq ?c 7))))
=>
  (modify ?cl (type A))
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
  (test (not (or (and (eq ?r 1) (eq ?c 1)) (and (eq ?r 6) (eq ?c 6)) (and (eq ?r 1) (eq ?c 6)) (and (eq ?r 6) (eq ?c 1)))))
=>
  (bind ?dist-top-left (sqrt (+ (** (- 0 ?r) 2) (** (- 0 ?c) 2))) )
  (bind ?dist-top-right (sqrt (+ (** (- 0 ?r) 2) (** (- 7 ?c) 2))) )
  (bind ?dist-bottom-left (sqrt (+ (** (- 7 ?r) 2) (** (- 0 ?c) 2))) )
  (bind ?dist-bottom-right (sqrt (+ (** (- 7 ?r) 2) (** (- 7 ?c) 2))) )
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
  ?b <- (white-border-counter (step ?s) (index ?i) (count ?cnt)) 
  (test (or (eq ?i ?c) (eq ?i ?r)))
  (test (< ?cnt 3))
=> 
  (bind ?new-cost 20)
  (modify ?cl (nearCorner ?new-cost))
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
