(define Trivial (make-class
                 Root
                 ()
                 (init ())))

(define Copy (make-class
                 Root
                 ()
                 (init ())))

(eq? Trivial Copy)
