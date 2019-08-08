;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(define foo
  (lambda ()
    (integers-from 23)))

(narc-skip
 (23 (foo))
 (24 ?)
 (25 ?)
 (0  0)
 (26 ?))

(narc-report)
