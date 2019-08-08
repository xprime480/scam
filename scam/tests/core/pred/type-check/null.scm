;;; Test the null? function
;;;

(narc-label "Type Checker of Null")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (null?)))
 (:args (test-err-cat (null? '() '())))

 (#f (null? ()))
 (#t (null? 2)))

(narc-report)
