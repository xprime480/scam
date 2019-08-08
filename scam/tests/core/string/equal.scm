;;; Test string equality with string=?.
;;; This operator is case sensitive
;;;

(narc-label "String equality")

(load "lib/test/test-handler.scm")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")

(narc-expect
 (:args (test-err-cat (string=? s1 s2 err)))
 (:args (test-err-cat (string=?)))
 (:args (test-err-cat (string=? s1)))
 (#f    (string=? s1 s2))
 (#t    (string=? s2 s2))
 (#f    (string=? s2 s4))
 (#f    (string=? s2 s3))               ; strings are case sensitive
 (#t    (string=? s2 s2 s2 s2 s2)))

(narc-report)
