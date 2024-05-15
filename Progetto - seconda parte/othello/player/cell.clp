(defmodule CELL (export deftemplate ?ALL))

(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot nearCorner)
  (slot content (allowed-values empty white black))
)