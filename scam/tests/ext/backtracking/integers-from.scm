;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers From")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(define *v* (integers-from 23))

(narc-expect
 (1 1))

(narc-skip
 (23 *v*)
 (24 (begin ? *v*))
 (25 (begin ? *v*))
 (0 0)
 (26 (begin ? *v*))
 (27 (begin ? *v*)))

(narc-report)
