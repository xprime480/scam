
(let ((result (instantiate '(:Q :Q))))
  (and (cons? result)
       (let ((r2 (car result))
             (r3 (car (cdr result))))
         (and (keyword? r2)
              (keyword? r3)
              (eq? r2 r3)
              (not (eq? :Q r2))))))
