;;; Call parent function directly
;;;

(narc-label "Call Parent Function Parent Directly")

(define Parent (make-class
                Root
                ()
                (get () -1)))

(define Trivial (make-class
                 Parent
                 ()))

(define obj (Trivial))

(narc-expect
 (-1 (obj get)))

(narc-report)
