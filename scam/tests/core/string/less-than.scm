(import (test narc))

(narc-label "String Less Than")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")

(narc-expect
 (#t (string<? s1 s2))
 (#f (string<? s2 s2))
 (#t (string<? s2 s4))
 (#f (string<? s2 s3))
 (#t (string<? s3 s2))
 (#t (string<? s1 s3 s2 s4)))

(narc-catch
 (:args (string<? s1 s2 err))
 (:args (string<?))
 (:args (string<? s1)))

(narc-report)
