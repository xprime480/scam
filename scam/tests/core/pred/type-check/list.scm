;;; Test the list? function
;;;

(narc-label "Type Checker of Lists")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (list?)))
 (:args (test-err-cat (list? '(a . b) (list 1 2 3))))

 (#f (list? (cons 1 2)))
 (#t (list? (list 1 2 3)))
 (#f (list? 2))
 (#t (list? '()))
 (#f (list? '(1 2 3 4 5 6 . 7)))))

(narc-report)
