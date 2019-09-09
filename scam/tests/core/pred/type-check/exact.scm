(import (only (scheme base) exact? inexact?)
        (test narc))

(narc-label "Type Checker of Exact Numbers")

(narc-expect
 (#t (exact? 123))
 (#t (exact? 11/2))
 (#f (exact? 5.5))
 (#t (exact? 2+3i))
 (#f (exact? 2.0+3i)))

(narc-catch
 (:args (exact?))
 (:args (exact? 1 2)))

;; (:args (exact? 'a-symbol))  throws an exception
;; (:args (inexact? 'a-symbol))

(narc-expect
 (#f (inexact? 123))
 (#f (inexact? 11/2))
 (#t (inexact? 5.5))
 (#f (inexact? 2+3i))
 (#t (inexact? 2.0+3i)))

(narc-catch
 (:args (inexact?))
 (:args (inexact? 1 2)))

(narc-report)
