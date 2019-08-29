(import (lib test narc))

(narc-label "Define Syntax With Arguments")

(define-syntax sum-plus-one
  (syntax-rules ()
    ((sum-plus-one a b)
     (+ a b 1))))

(narc-expect
 (15 (sum-plus-one 2 (* 3 4))))

(narc-report)
