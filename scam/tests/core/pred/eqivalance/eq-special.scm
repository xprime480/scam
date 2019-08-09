;;; Test the eq? function for special cases
;;;

(narc-label "Eq? Special")

(narc-catch
 (:args (eq?))
 (:args (eq? #t))
 (:args (eq? #t #t #t)))

(narc-report)
