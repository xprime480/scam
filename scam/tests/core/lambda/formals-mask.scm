(import (only (scheme base) /)
        (test narc))

(narc-label "Lambda Formals Override Env")

(define x 0.0)
(define (f x)
  (/ 1.0 x))

(narc-expect
 (#i1/2 (f 2)))

(narc-report)
