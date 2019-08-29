(import (lib test narc))

(narc-label "CMP = Bad Args")

(narc-catch
 (:args (= #t))
 (:args (= 3 "x")))

(narc-report)

