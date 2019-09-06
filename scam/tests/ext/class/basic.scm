(import (test narc))

(narc-label "Class Predicates")

(define Trivial (make-class
                 Root
                 ()
                 (init ())))

(define instance (Trivial))

(narc-expect
 (#t (procedure? make-class))
 (#t (procedure? Trivial))
 (#t (procedure? instance))
 (#f (class? make-class))
 (#t (class? Trivial))
 (#f (class? instance))
 (#f (instance? make-class))
 (#f (instance? Trivial))
 (#t (instance? instance)))

(narc-report)
