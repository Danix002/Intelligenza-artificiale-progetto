(defmodule CONTROL (export ?ALL))

(deftemplate time
  (slot step)
)



(defrule str (declare (salience 10))
=>
  (set-strategy depth)
)
