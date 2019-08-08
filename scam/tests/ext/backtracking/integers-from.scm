;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(narc-skip
 (1                 (integers-between 1 3))
 (2                 ?)
 (3                 ?)
 ("No more choices" ?))

(narc-report)
