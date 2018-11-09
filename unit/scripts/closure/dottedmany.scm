(define f (lambda (x . y)
            y))
(f 1 2 (+ 2 2) #t)
