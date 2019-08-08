;;; Test the boolean=? function
;;;

(narc-label "Equality of Booleans")

(load "lib/prelude.scm")

(narc-catch
 (:args (boolean=?))
 (:args (boolean=? #t)))

(narc-expect
 (#t (boolean=? #t #t #t))
 (#t (boolean=? #f #f #f #f))
 (#f (boolean=? #t #f #t))
 (#f (boolean=? #t 2)))

(narc-report)
