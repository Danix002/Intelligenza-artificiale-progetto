(defmodule MOVE (export deftemplate ?ALL))

(deftemplate move
   (slot step)
   (slot row)
   (slot col)
   (slot cost)
   (slot confidence)
)

(defrule guess-move-random
   ?t <- (time (step ?s))
   (cell (step ?s) (row ?r) (col ?c) (content empty))
=>
   (bind ?s3 (+ ?s 1))
   (assert (move (step ?s3) (row ?r) (col ?c)))
   (modify ?t (step ?s))
) 

(defrule forget-past-moves  
   (time (step ?s)) 
   ?m <- (move (step ?s3&:(< ?s3 ?s)) (row ?r) (col ?c))
=>
   (retract ?m)
)