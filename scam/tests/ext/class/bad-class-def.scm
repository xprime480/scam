;;; Malformed class definition due to an expected functional form
;;; that is not a procedure definition.
;;;

(narc-label "Malformed Class Definition")

(narc-catch
 (:args (make-class Root
                    (val)
                    (init (v) (set! val v))
                    (foo))))

(narc-report)
