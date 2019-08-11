;;; Undefine only affects current scope
;;;

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