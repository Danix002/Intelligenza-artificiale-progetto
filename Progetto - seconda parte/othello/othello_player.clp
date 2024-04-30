(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot content (allowed-values empty white black))
)

(deftemplate move
   (slot step)
   (slot row)
   (slot col)
)

(deftemplate time
  (slot step)
)


(deffacts initial-time
   (time (step -1))
)

(defrule str (declare (salience 10))
=>
  (set-strategy depth)
 )

(defrule guess-move
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





