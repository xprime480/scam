(import (lib test narc))

(narc-label "Malformed Lambda Expressions")

(narc-catch
 (:args (lambda))
 (:args (lambda 2 (+ 2 2)))
 (:args (lambda (:keyword) (+ 2 2)))
 (:args (lambda (x x) (* x 2)))
 (:args (lambda (x . x) (* x 2)))
 (:args (lambda (ok . :keyword) (+ 2 2))))

(narc-report)
