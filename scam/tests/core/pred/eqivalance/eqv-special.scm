;;; Test the eqv? function for special cases
;;;

(narc-label "Eqv? Special")

(narc-catch
 (:args (eqv?))
 (:args (eqv? #t))
 (:args (eqv? #t #t #t)))

(narc-report)
