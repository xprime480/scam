;;; Distinct? function
;;;

(narc-label "Distinct?")

(load "lib/prelude.scm")

(narc-expect
 (#t (distinct? (list)))
 (#t (distinct? (list 1)))
 (#t (distinct? (list 1 5 #f 'cat)))
 (#f (distinct? (list 1 5 #f 'cat 5))))

(narc-report)