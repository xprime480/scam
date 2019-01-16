
(let ((result (instantiate '(:Q))))
  (and (cons? result)
       (let ((r2 (car result)))
         (and (keyword? r2)
              (not (eq? :Q r2))))))
