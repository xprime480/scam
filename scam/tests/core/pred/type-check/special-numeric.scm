(import (lib test narc))

(narc-label "Special Numeric Predicates")

(narc-expect
 (#t  (nan? +nan.0))
 (#t  (nan? -nan.0))
 (#t  (nan? 2-nan.0i))
 (#t  (nan? +nan.0+3i))
 (#f  (nan? 3))
 (#f  (nan? :nope))

 (#t  (infinite? +inf.0))
 (#t  (infinite? -inf.0))
 (#t  (infinite? 1-inf.0i))
 (#t  (infinite? +inf.0+7i))
 (#f  (infinite? +nan.0))
 (#f  (infinite? 3))
 (#f  (infinite? :nope))

 (#t  (finite? 3))
 (#t  (finite? 1.75))
 (#t  (finite? 1-7i))
 (#f  (finite? +nan.0))
 (#f  (finite? -nan.0))
 (#f  (finite? +inf.0))
 (#f  (finite? -inf.0))
 (#f  (finite? :nope)))

(narc-report)
