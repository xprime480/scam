(import (lib test narc))

(narc-label "Undefine Scope")

(define test 1)

(narc-expect
 (1 test))

(narc-catch
 (:env (let ()
         (undefine test)
         test)))

(narc-expect
 (1 test))

(narc-report)
