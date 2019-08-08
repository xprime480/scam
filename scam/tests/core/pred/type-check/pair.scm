;;; Test the pair? function
;;;

(narc-label "Type Checker of Pairs")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (pair?)))
 (:args (test-err-cat (pair? '(a . b) (list 1 2 3))))

 (#t (pair? (cons 1 2)))
 (#t (pair? (list 1 2 3)))
 (#f (pair? 2))
 (#f (pair? '())))

(narc-report)
