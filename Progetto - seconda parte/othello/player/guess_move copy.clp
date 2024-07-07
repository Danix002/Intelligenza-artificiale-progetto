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

(deftemplate white-border-counter
  (slot step)
  (slot position) 
  (slot index) 
  (slot count))

(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot nearCorner)
  (slot content (allowed-values empty white black))
  (slot type (allowed-values empty C A B X))
)

;inizializzazione dei counter
(defrule initialize-white-border-counters (declare (salience 12))
  ?t <- (time (step ?s))
  (not (white-border-counter (step ?s) (position row) (index 0) (count ?cnt0)))
  (not (white-border-counter (step ?s) (position row) (index 7) (count ?cnt7)))
  (not (white-border-counter (step ?s) (position col) (index 1) (count ?cnt1)))
  (not (white-border-counter (step ?s) (position col) (index 6) (count ?cnt6)))
=>
  (assert (white-border-counter (step ?s) (position row) (index 0) (count 0)))
  (assert (white-border-counter (step ?s) (position row) (index 7) (count 0)))
  (assert (white-border-counter (step ?s) (position col) (index 1) (count 0)))
  (assert (white-border-counter (step ?s) (position col) (index 6) (count 0)))
)


; counter righe
(defrule count-white-border-cells-row-0 (declare (salience 11))
  ?c <- (cell (row 0) (col ?col) (content white))
  ?b <- (white-border-counter (position row) (index 0) (count ?cnt))
=>
  (modify ?b (count (+ ?cnt 1)))
)

(defrule count-white-border-cells-row-7 (declare (salience 11))
  ?c <- (cell (row 7) (col ?col) (content white))
  ?b <- (white-border-counter (position row) (index 7) (count ?cnt))
=>
  (modify ?b (count (+ ?cnt 1)))
)

; counter colonne 
(defrule count-white-border-cells-col-0 (declare (salience 11))
  ?t <- (time (step ?s))
  ?c <- (cell (row ?row) (col 0) (content white))
  ?b <- (white-border-counter (step ?s) (position col) (index 1) (count ?cnt))
=>
  (modify ?b (count (+ ?cnt 1)))
)

(defrule count-white-border-cells-col-7 (declare (salience 11))
  ?c <- (cell (row ?row) (col 6) (content white))
  ?b <- (white-border-counter (position col) (index 6) (count ?cnt))
=>
  (modify ?b (count (+ ?cnt 1)))
)

(defrule forget-past-moves  
   (time (step ?s)) 
   ?m <- (move (step ?s3&:(< ?s3 ?s)) (row ?r) (col ?c))
=>
   (retract ?m)
)

(defrule reset-white-border-counter-row
  ?t <- (time (step ?s))
  (not (white-border-counter (step ?s3&:(< ?s3 ?s)) (position row) (count 0)))
=>
  (retract ?t)
)

(defrule reset-white-border-counter-col
  ?t <- (time (step ?s))
  (not (white-border-counter (step ?s3&:(< ?s3 ?s)) (position col ) (count 0)))
=>
  (retract ?t)
)


(defrule update-cost-of-cell (declare (salience 9))
   ?t <- (time (step ?s))
   ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty) (type empty))
   (test (not (and (eq ?r 1) (eq ?c 1))))
   (test (not (and (eq ?r 6) (eq ?c 6))))
   (test (not (and (eq ?r 1) (eq ?c 6))))
   (test (not (and (eq ?r 6) (eq ?c 1))))
=> 
   (bind ?dist-top-left (sqrt (+ (** (- 0 ?r) 2) (** (- 0 ?c) 2))) )
   (bind ?dist-top-right (sqrt (+ (** (- 0 ?r) 2) (** (- 7 ?c) 2))) )
   (bind ?dist-bottom-left (sqrt (+ (** (- 7 ?r) 2) (** (- 0 ?c) 2))) )
   (bind ?dist-bottom-right (sqrt (+ (** (- 7 ?r) 2) (** (- 7 ?c) 2))) )
   
   (bind ?new-cost (min ?dist-top-left ?dist-top-right ?dist-bottom-left ?dist-bottom-right))

   (modify ?cl (nearCorner ?new-cost) )
)


(defrule set-cell-type-X (declare (salience 10))
   ?t <- (time (step ?s))
   ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty) (type empty))
   (test ( or (and (eq ?r 1) (eq ?c 1)) (and (eq ?r 6) (eq ?c 6)) (and (eq ?r 6) (eq ?c 1)) (and (eq ?r 1) (eq ?c 6)) ) )
=>
   (modify ?cl (type X))
)

(defrule set-cell-type-C (declare(salience 10))
    ?t <- (time (step ?s))
    ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty) (type empty))
    (test ( or (and (eq ?r 1) (eq ?c 0)) 
               (and (eq ?r 0) (eq ?c 1)) 
               (and (eq ?r 0) (eq ?c 6)) 
               (and (eq ?r 1) (eq ?c 7)) 
               (and (eq ?r 6) (eq ?c 0)) 
               (and (eq ?r 7) (eq ?c 1)) 
               (and (eq ?r 7) (eq ?c 6))
               (and (eq ?r 6) (eq ?c 7))
               ))
   => 
    (modify ?cl (type C))
)



(defrule update-cost-of-certain-cell-X (declare (salience 8))
   ?t <- (time (step ?s))
   ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty)  (type X))
=> 
   (bind ?new-cost 20)
   (modify ?cl (nearCorner ?new-cost) )
)

(defrule update-cost-of-certain-cell-C (declare (salience 8))
   ?t <- (time (step ?s))
   ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty) (type C))
   ?b <- (white-border-counter  (index ?i) (count ?cnt)) 
   (test (or (eq ?i ?c) (eq ?i ?r)))
   (test (< ?cnt 3))
=> 
   (bind ?new-cost 20)
   (modify ?cl (nearCorner ?new-cost) )
)


(defrule guess-move 
   ?t <- (time (step ?s))
   (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty))
=>
   (bind ?s3 (+ ?s 1))
   (assert (move (step ?s3) (row ?r) (col ?c) (cost ?a) ))
   (modify ?t (step ?s))
) 



(defrule str (declare (salience 10))
=>
  (set-strategy depth)
)