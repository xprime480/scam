(define Trivial (make-class
                 Root
                 ()
                 (init ())))

(define Other (make-class
                 Root
                 (x)
                 (init (q c) (assign! x (+ 3 q c)))))

(equal? Trivial Other)
