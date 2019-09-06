(import (test narc))

(narc-label "(CI) String Less Than")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")
(define s5 "GIRL")

(narc-expect
 (#t     (string-ci<? s1 s2))
 (#f     (string-ci<? s2 s2))
 (#t     (string-ci<? s2 s4))
 (#f     (string-ci<? s2 s3))
 (#f     (string-ci<? s3 s2))
 (#t     (string-ci<? s1 s2 s5)))

(narc-catch
 (:args  (string-ci<? s1 s2 err))
 (:args  (string-ci<?))
 (:args  (string-ci<? s1)))

(narc-report)
