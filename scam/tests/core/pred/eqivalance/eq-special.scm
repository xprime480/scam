(import (lib test narc))

(narc-label "Eq? Special")

(narc-catch
 (:args (eq?))
 (:args (eq? #t))
 (:args (eq? #t #t #t)))

(narc-report)
