;;; Eval self-evaluating forms
;;;

(narc-label "Basic Eval")

(define x 1)

(narc-expect
 (2 (eval 2))
 (:foo (eval :foo))
 ('() (eval '()))
 ('(2 4) (let ((x 2))
           (list x
                 (eval `(+ x 3))))))

(narc-report)
