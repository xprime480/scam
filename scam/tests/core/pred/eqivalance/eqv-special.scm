;;; Test the eqv? function for special cases
;;;

(narc-label "Eqv? Special")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (eqv?)))
 (:args (test-err-cat (eqv? #t)))
 (:args (test-err-cat (eqv? #t #t #t))))

(narc-report)
