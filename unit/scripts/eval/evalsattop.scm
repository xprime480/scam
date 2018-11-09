(define x 1)
(let ((x 2))
  (list x
        (eval `(+ x 3))))
