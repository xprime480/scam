(import (test narc))

(narc-label "Type Checker of Keywords")

(narc-expect
 (#t (keyword? :basic))
 (#f (keyword? 'a-symbol))
 (#f (keyword? "a string")))

(narc-catch
 (:args (keyword?))
 (:args (keyword? :one :two)))

(narc-report)
