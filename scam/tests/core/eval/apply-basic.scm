;;; Simple Apply
;;;

(narc-label "Simple Apply")

(narc-expect
 (99 (apply * (list 3 33)))
 (13 (apply (lambda (x y) (+ x 2 (* 2 y))) '(1 5))))

(narc-report)
