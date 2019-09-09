(import (only (scheme base) raise)
        (test narc))

(narc-label "Raise")

(narc-catch
 (:args (raise))
 (:args (raise 1 2 3))

 ("Error" (raise "Error"))
 (2       (raise 2)))

(narc-report)
