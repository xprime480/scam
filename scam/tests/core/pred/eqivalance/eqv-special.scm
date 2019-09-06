(import (test narc))

(narc-label "Eqv? Special")

(narc-catch
 (:args (eqv?))
 (:args (eqv? #t))
 (:args (eqv? #t #t #t)))

(narc-report)
