
(define **cont** ())

(+ 1 (call/cc
      (lambda (k)
        (assign! **cont** k))))

(+ 2 3 (* 4 (/ 1 (**cont** 0)) (/ 2 0)))
