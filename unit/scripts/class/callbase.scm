
(define Parent (make-class
                Root
                ()
                (get () -1)))

(define Trivial (make-class
                 Parent
                 ()))

((Trivial) get)
