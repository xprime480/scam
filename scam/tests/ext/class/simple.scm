(import (test narc))

(narc-label "Simple Constant Class")

(define SimpleConstant (make-class
                        Root
                        (val)
                        (init (v) (set! val v))
                        (get () val)))
(define k 21)

(define answer (SimpleConstant (* k 2)))

(narc-expect
 (42 (answer get)))

(narc-report)
