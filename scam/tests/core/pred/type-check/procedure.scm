(import (lib test narc))

(narc-label "Type Checker of Procedures")

(narc-expect
 (#t (procedure? car))
 (#f (procedure? 'car))
 (#t (procedure? (lambda (x) (* x x))))
 (#f (procedure? '(lambda (x) (* x x))))
 (#t (call/cc procedure?)))

(narc-catch
 (:args (procedure?))
 (:args (procedure? car cdr)))

(narc-report)
