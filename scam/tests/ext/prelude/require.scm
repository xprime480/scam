;;; Require function
;;;

(narc-label "Require")

(load "lib/prelude.scm")

(narc-expect
 (#t                (require #t))
 ("No more choices" (require #f)))

(narc-report)
