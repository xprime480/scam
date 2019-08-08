;;; Test the boolean? function
;;;

(narc-label "Type Checker of Booleans")

(load "lib/test/test-handler.scm")

(narc-expect
 (:args (test-err-cat (boolean?)))
 (:args (test-err-cat (boolean? #t #f)))

 ('(#t #t #f #t) (list
                  (boolean? #t)
                  (boolean? #f)
                  (boolean? 2)
                  (boolean? (> 2 3)))))

(narc-report)
