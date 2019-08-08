;;; Test the port? function
;;;

(narc-label "Type Checker of Ports")


(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (port?)))
 (:args (test-err-cat (port? '(a . b) (list 1 2 3))))

 (#f (port? 2))
 (#t (port? (open-input-string ""))))

(narc-report)
