(import (only (scheme base) /)
        (test narc))

(narc-label "Lambda Wrong Arg Count")

(define (f x)
  (/ 1.0 x))

(narc-catch
 (:args (f))
 (:args (f 1 2 3 4)))

(narc-report)
