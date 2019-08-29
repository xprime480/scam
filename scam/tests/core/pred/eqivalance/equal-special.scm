(import (lib test narc))

(narc-label "Equal? Special")

(narc-catch
 (:args (equal?))
 (:args (equal? #t))
 (:args (equal? #t #t #t)))

(narc-report)
