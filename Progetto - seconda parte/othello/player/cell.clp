(deftemplate cell
  (slot step)
  (slot row)
  (slot col)
  (slot content (allowed-values empty white black))
)