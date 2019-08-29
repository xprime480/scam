(import (lib test narc))

(narc-label "Undefine")

(define test 1)

(narc-expect
 (1 test))

(undefine test)

(narc-catch
 (:eval test))

(narc-report)
