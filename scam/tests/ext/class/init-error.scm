(import (lib test narc))

(narc-label "Class Init Error")

(define Trivial (make-class
                 Root
                 (n)
                 (init (v) (set! n v))))
(narc-catch
 (:args (Trivial (/ 1 0))))

(narc-report)
