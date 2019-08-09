;;; Require function
;;;

(narc-label "Require")

(load "lib/prelude.scm")

(narc-skip
 (#t                (require #t))
 ("No more choices" (require #f)))

(narc-expect
 (1 1))

(narc-report)
