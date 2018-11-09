(define f ())
(let ((y 5))
  (assign! f (lambda (x)
               (* x y))))

(f (+ 1 3))
