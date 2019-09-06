(import (test narc))

(narc-label "Malformed Class Definition")

(narc-catch
 (:args (make-class Root
                    (val)
                    (init (v) (set! val v))
                    (foo))))

(narc-report)
