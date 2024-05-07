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
