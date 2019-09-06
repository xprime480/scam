(import (test narc))

(narc-label "Parent Calls Self Function")

(define Parent (make-class
                Root
                ()
                (jarthur () (* (self doodle) 17))
                (doodle () 1)))

(define Trivial (make-class
                 Parent
                 ()
                 (get () (parent jarthur))
                 (doodle () 2)))

(define obj (Trivial))

(narc-expect
 (34 (obj get)))

(narc-report)
