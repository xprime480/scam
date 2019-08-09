;;; Test string greater than with string>?
;;; This operator is case sensitive
;;;

(narc-label "String Greater Than")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")

(narc-expect
 (#f     (string>? s1 s2))
 (#f     (string>? s2 s2))
 (#f     (string>? s2 s4))
 (#t     (string>? s2 s3))
 (#f     (string>? s3 s2))
 (#t     (string>? s4 s2 s3 s1)))

(narc-catch
 (:args  (string>? s1 s2 err))
 (:args  (string>?))
 (:args  (string>? s1)))

(narc-report)
