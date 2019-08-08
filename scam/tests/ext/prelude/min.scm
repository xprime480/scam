;;; Minimum function
;;;

(narc-label "Min")

(load "lib/prelude.scm")

(narc-expect
 (-123   (min 123 -123))
 (17.5   (min 42.01 17.5))
 (-9.999 (min -5 -9.999)))

(narc-report)
