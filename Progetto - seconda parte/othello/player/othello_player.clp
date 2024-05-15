;(defmodule RULES (import MOVE ?ALL) (import CONTROL ?ALL) (import CELL ?ALL))

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

(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot nearCorner)
  (slot content (allowed-values empty white black))
)



(defrule forget-past-moves  
   (time (step ?s)) 
   ?m <- (move (step ?s3&:(< ?s3 ?s)) (row ?r) (col ?c))
=>
   (retract ?m)
)

(defrule update-cost-of-cell (declare (salience 10))
   ?t <- (time (step ?s))
   ?cl <- (cell (step ?s) (row ?r) (col ?c) (nearCorner ?a) (content empty))
=> 
   (bind ?dist-top-left (sqrt (+ (** (- 0 ?r) 2) (** (- 0 ?c) 2))) )
   (bind ?dist-top-right (sqrt (+ (** (- 0 ?r) 2) (** (- 7 ?c) 2))) )
   (bind ?dist-bottom-left (sqrt (+ (** (- 7 ?r) 2) (** (- 0 ?c) 2))) )
   (bind ?dist-bottom-right (sqrt (+ (** (- 7 ?r) 2) (** (- 7 ?c) 2))) )
   
   (bind ?new-cost (min ?dist-top-left ?dist-top-right ?dist-bottom-left ?dist-bottom-right))

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





