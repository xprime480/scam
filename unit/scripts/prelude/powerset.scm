
(let ((pset (power-set '(1 2))))
  (list
   (member? '() pset)
   (member? '(1) pset)
   (member? '(2) pset)
   (member? '(1 2) pset)))

