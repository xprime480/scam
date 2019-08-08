;;; Test operator that produce errors
;;;

(narc-label "Math Errors")

(load "lib/test/test-handler.scm")

(narc-catch
 (:args  (test-err-cat (+ 2 #f)))
 (:args  (test-err-cat (- :keyword)))
 (:args  (test-err-cat (* 2 #f)))
 (:args  (test-err-cat (/ 2 #f)))
 (:args  (test-err-cat (/ 2 0)))
 (:args  (test-err-cat (% 2 0)))
 (:args  (test-err-cat (+ (* 2 3) (/ 1 (+ 5 -5)) (- 3)))))

;;(% +i +i)  this is an ill-behaved expression -- make it a simple error

(narc-report)
