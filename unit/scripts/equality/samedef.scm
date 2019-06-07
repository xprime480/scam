(define Trivial (make-class
                 Root
                 ()
                 (init ())))

(define Copy (make-class
                 Root
                 ()
                 (init ())))

(equal? Trivial Copy)
