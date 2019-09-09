(import (only (scheme char) string-foldcase string-upcase string-downcase)
        (test narc))

(narc-label "Change String Case")

(define err :not-a-string)
(define empty "")
(define num-string "123")
(define aBcD "aBcD")

(narc-expect
 (""     (string-upcase empty))
 ("123"  (string-upcase num-string))
 ("ABCD" (string-upcase aBcD))

 (""     (string-downcase empty))
 ("123"  (string-downcase num-string))
 ("abcd" (string-downcase aBcD))

 (""     (string-foldcase empty))
 ("123"  (string-foldcase num-string))
 ("abcd" (string-foldcase aBcD))
 ("aBcD" aBcD)) ;; original is unchanged

(narc-catch
 (:args  (string-upcase err))
 (:args  (string-downcase err))
 (:args  (string-foldcase err)))

(narc-report)
