;;; Assign to symbol in enclosing scope
;;;

(narc-label "Assign Outer Scope")

(define x (- 3 2))
(let ()
  (set! x 77))

(narc-expect
 (77 x))

(narc-report)