(import (test narc))

(narc-label "Lambda With Symbol Formals")

(define (f . x)
  x)

(narc-expect
 ('()        (f))
 ('(5)       (f 5))
 ('(5 10 15) (f 5 10 15)))

(narc-report)
