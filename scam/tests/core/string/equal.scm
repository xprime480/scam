(import (only (scheme base) string=?)
        (test narc))

(narc-label "String equality")

(define err :not-a-string)
(define s1 "")
(define s2 "boy")
(define s3 "BoY")
(define s4 "girl")

(narc-expect
 (#f    (string=? s1 s2))
 (#t    (string=? s2 s2))
 (#f    (string=? s2 s4))
 (#f    (string=? s2 s3))               ; strings are case sensitive
 (#t    (string=? s2 s2 s2 s2 s2)))

(narc-catch
 (:args (string=? s1 s2 err))
 (:args (string=?))
 (:args (string=? s1)))

(narc-report)
