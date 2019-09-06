(import (test narc))

(narc-label "Apply")

(narc-expect
 (99 (apply * (list 3 33)))
 (13 (apply (lambda (x y) (+ x 2 (* 2 y))) '(1 5))))

(narc-catch
 (:args (apply))
 (:args (apply * 3 33)))

(narc-report)
