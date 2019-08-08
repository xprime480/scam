;;; Test the procedure? function
;;;

(narc-label "Type Checker of Procedures")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (procedure?)))
 (:args (test-err-cat (procedure? car cdr)))

 (#t (procedure? car))
 (#f (procedure? 'car))
 (#t (procedure? (lambda (x) (* x x))))
 (#f (procedure? '(lambda (x) (* x x))))
 (#t (call/cc procedure?)))

(narc-report)
