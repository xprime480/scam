;;; Test string greater than or equal with string-ci>=?
;;; This operator is not case sensitive
;;;

(narc-label "(CI) String Greater Than or Equal")

(load "lib/test/test-handler.scm")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")
(define s5 "Girl")

(narc-expect
 (:args  (test-err-cat (string-ci>=? s1 s2 err)))
 (:args  (test-err-cat (string-ci>=?)))
 (:args  (test-err-cat (string-ci>=? s1)))
 (#f     (string-ci>=? s1 s2))
 (#t     (string-ci>=? s2 s2))
 (#f     (string-ci>=? s2 s4))
 (#t     (string-ci>=? s2 s3))
 (#t     (string-ci>=? s3 s2))
 (#t     (string-ci>=? s5 s2))
 (#t     (string-ci>=? s5 s2 s1)))

(narc-report)
