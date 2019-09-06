(import (test narc))

(narc-label "Math Errors")

(narc-catch
 (:args  (+ 2 #f))
 (:args  (- :keyword))
 (:args  (* 2 #f))
 (:args  (/ 2 #f))
 (:args  (/ 2 0))
 (:args  (% 2 0))
 (:args  (+ (* 2 3) (/ 1 (+ 5 -5)) (- 3))))

;;(% +i +i)  this is an ill-behaved expression -- make it a simple error

(narc-report)
