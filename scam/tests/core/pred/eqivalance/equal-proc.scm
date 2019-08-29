(import (lib test narc))

(narc-label "Equal? Proc")

(define (proc1 a)
  a)
(define (proc2 a)
  a)

(narc-expect
 (#t (equal? proc1 proc1))
 (#f (equal? proc1 proc2)))

(narc-report)
