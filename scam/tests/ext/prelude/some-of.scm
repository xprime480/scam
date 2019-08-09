;;; Some-of function
;;;

(narc-label "Some-Of")

(load "lib/prelude.scm")

(narc-skip
 ("No more choices" (some-of '()))
 ('(1)              (some-of '(1)))
 ("No more choices" ?))

(narc-expect
 (1 1))

(narc-report)
