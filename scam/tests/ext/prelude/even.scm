;;; Even? function
;;;

(narc-label "Even?")

(load "lib/prelude.scm")

(narc-expect
 (#t (even? -2))
 (#t (even? 0))
 (#t (even? 2))
 (#t (even? 12398234))
 (#f (even? -3))
 (#f (even? 3))
 (#f (even? 82383))
 (#t (even? 2.0))
 (#f (even? #t))
 (#f (even? "Silly, strings don't have parity")))

(narc-report)
