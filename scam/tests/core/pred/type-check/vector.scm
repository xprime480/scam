(import (lib test narc))

(narc-label "Type Checker of Vectors")

(narc-expect
 (#t (vector? #(1 2 3)))
 (#f (vector? (list 'a 'b)))
 (#f (vector? 2)))

(narc-catch
 (:args (vector?))
 (:args (vector? #() #())))

(narc-report)
