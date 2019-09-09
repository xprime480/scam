(import (only (scheme base) -)
        (test narc))

(narc-label "Assign Outer Scope")

(define x (- 3 2))
(let ()
  (set! x 77))

(narc-expect
 (77 x))

(narc-report)
