(import (only lib/prelude xor)
        (lib test narc))

(narc-label "Basic Logic")

(narc-expect
 ('(1 -1 () 6) (list (if #t +1 -1)
                     (if #f +1 -1)
                     (if #f +1)
                     (if #t (* 2 3) (/ 1 0))))

 ('(#t 3 #f 3 #f 4 2) (list (and)
                            (and 3)
                            (and #f)
                            (and #t #t 3)
                            (and #f (/ 1 0))
                            (and 2 (and 3 4))
                            (and (and 3 4) 2)))

 ('(#f 3 #f #t #t 3 2) (list (or)
                             (or 3)
                             (or #f)
                             (or #t #t 3)
                             (or #t (/ 1 0))
                             (or #f (or 3 4))
                             (or (or #f #f) 2)))

 ('(#f #t) (list (not 3)
                 (not #f)))

 ('(#t #t #f #f) (list (xor #t #f)
                       (xor #f #t)
                       (xor #t #t)
                       (xor #f #f))))

(narc-catch
 (:args (if #t))
 (:args (if #t 1 2 3 4 5 ()))
 (:args (if (/ 1 0) (* 2 3) (/ 1 0)))
 (:args (if "strings are true" (/ 1 0))))

(narc-report)
