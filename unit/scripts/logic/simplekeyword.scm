
(let ((result (instantiate :X)))
  (and (keyword? result)
       (not (eq? :X result))))
