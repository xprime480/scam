;;; Test string-upcase, string-downcase, string-foldcase
;;;

(narc-label "Change String Case")

(load "lib/prelude.scm")
(load "lib/test/test-handler.scm")

(define err :not-a-string)
(define empty "")
(define num-string "123")
(define aBcD "aBcD")

(narc-expect
 (:args  (test-err-cat (string-upcase err)))
 (""     (string-upcase empty))
 ("123"  (string-upcase num-string))
 ("ABCD" (string-upcase aBcD))

 (:args  (test-err-cat (string-downcase err)))
 (""     (string-downcase empty))
 ("123"  (string-downcase num-string))
 ("abcd" (string-downcase aBcD))

 (:args  (test-err-cat (string-foldcase err)))
 (""     (string-foldcase empty))
 ("123"  (string-foldcase num-string))
 ("abcd" (string-foldcase aBcD))
 ("aBcD" aBcD)) ;; original is unchanged

(narc-report)
