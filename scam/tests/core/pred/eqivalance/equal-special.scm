;;; Test the equal? function for special cases
;;;

(narc-label "Equal? Special")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (equal?)))
 (:args (test-err-cat (equal? #t)))
 (:args (test-err-cat (equal? #t #t #t))))

(narc-report)
