;;; Class with no initialization functions
;;;
;;; Not sure if this is really a good test -- if you reference an
;;; unitialized variable, anything (almost) goes.
;;;

(narc-label "Class Without Init")

(define Trivial (make-class
                 Root
                 (n)
                 (get () n)))

(define obj (Trivial))

(narc-expect
 ('() (obj get)))

(narc-report)
