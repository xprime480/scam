;;; Test the require function in the prelude
;;;

(narc-label "Require")

(load "lib/prelude.scm")

(narc-skip
 (2 (let ((x (amb 1 2 3 4 5 6)))
      (begin
        (require (even? x))
        x)))
 (4 ?)
 (6 ?))

(narc-expect
 (1 1))

(narc-report)
