;;; Filter function
;;;

(narc-label "Filter")

(load "lib/prelude.scm")

(narc-expect
 ('()    (filter even? '()))
 ('(2 4) (filter even? '(1 2 3 4 5))))

(narc-report)
