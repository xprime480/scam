(import (test narc))

(narc-label "Define Syntax With Ellipsis")

(define-syntax dots-test
  (syntax-rules ()
    ((dots-test v ...)
     '((foo v) ...))))

(narc-expect
 ('((foo 1) (foo 2) (foo 3)) (dots-test 1 2 3)))

(narc-report)
