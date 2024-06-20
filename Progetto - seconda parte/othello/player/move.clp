(defmodule MOVE (export deftemplate ?ALL))

(deftemplate move
   (slot step)
   (slot row)
   (slot col)
   (slot cost)
   (slot confidence)
)



(defrule forget-past-moves  
   (time (step ?s)) 
   ?m <- (move (step ?s3&:(< ?s3 ?s)) (row ?r) (col ?c))
=>
   (retract ?m)
)