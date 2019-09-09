(import (test narc))

(narc-label "Lambda With Dotted Formals")

(define (f x . y)
  y)

(narc-expect
 ('()       (f 1))
 ('(2)      (f 1 2))
 ('(2 4 #t) (f 1 2 (+ 2 2) #t)))

(narc-report)
