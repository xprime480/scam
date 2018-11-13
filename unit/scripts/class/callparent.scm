
(define Parent (make-class
                Root
                ()
                (jarthur (n) (* n 17))))

(define Trivial (make-class
                 Parent
                 ()
                 (get () (parent jarthur 1))))

((Trivial) get)
