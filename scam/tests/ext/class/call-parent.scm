(import (lib test narc))

(narc-label "Call Function in Parent")

(define Parent (make-class
                Root
                ()
                (jarthur (n) (* n 17))))

(define Trivial (make-class
                 Parent
                 ()
                 (get () (parent jarthur 1))))

(define obj (Trivial))

(narc-expect
 (17 (obj get))
 (51 (obj jarthur 3)))

(narc-report)
