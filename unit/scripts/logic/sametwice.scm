
(let ((r1 (instantiate :X))
      (r2 (instantiate :X)))
  (and (keyword? r1)
       (keyword? r2)
       (not (eq? r1 :X))
       (not (eq? r2 :X))
       (not (eq? r1 r2))))
