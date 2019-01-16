
(let ((result (instantiate [7 :123])))
  (and (vector? result)
       (eq? 2 (length result))
       (let ((r2 (nth 0 result))
	     (r3 (nth 1 result)))
         (and (eq? r2 7)
	      (keyword? r3)
              (not (eq? :123 r3))))))
