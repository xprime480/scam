;;; Test the eq? function for special cases
;;;

(narc-label "Eq? Special")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (eq?)))
 (:args (test-err-cat (eq? #t)))
 (:args (test-err-cat (eq? #t #t #t))))

(narc-report)
