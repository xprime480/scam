(import (only (scheme base) * / not)
        (only (scam base) xor)
        (test narc))

(narc-label "Basic Logic")

(narc-expect
 (1   (if #t +1 -1))
 (-1  (if #f +1 -1))
 ('() (if #f +1))
 (6   (if #t (* 2 3) (/ 1 0)))

 (#t (and))
 (3  (and 3))
 (#f (and #f))
 (3  (and #t #t 3))
 (#f (and #f (/ 1 0)))
 (4  (and 2 (and 3 4)))
 (2  (and (and 3 4) 2))

 (#f (or))
 (3  (or 3))
 (#f (or #f))
 (#t (or #t #t 3))
 (#t (or #t (/ 1 0)))
 (3  (or #f (or 3 4)))
 (2  (or (or #f #f) 2))

 (#f (not 3))
 (#t (not #f))

 (#t (xor #t #f))
 (#t (xor #f #t))
 (#f (xor #t #t))
 (#f (xor #f #f)))

(narc-catch
 (:args (if #t))
 (:args (if #t 1 2 3 4 5 ()))
 (:args (if (/ 1 0) (* 2 3) (/ 1 0)))
 (:args (if "strings are true" (/ 1 0))))

(narc-report)
