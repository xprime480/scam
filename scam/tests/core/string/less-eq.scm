;;; Test string less than or equal with string<=?
;;; This operator is case sensitive
;;;

(narc-label "String Less Than or Equal")

(load "lib/test/test-handler.scm")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")

(narc-expect
 (:args  (test-err-cat (string<=? s1 s2 err)))
 (:args  (test-err-cat (string<=?)))
 (:args  (test-err-cat (string<=? s1)))
 (#t     (string<=? s1 s2))
 (#t     (string<=? s2 s2))
 (#t     (string<=? s2 s4))
 (#f     (string<=? s2 s3))
 (#t     (string<=? s3 s2))
 (#t     (string<=? s1 s3 s2 s2 s4)))

(narc-report)
