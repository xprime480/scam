(import (only (scheme base) cadr)
        (only (scam base) dict?)
        (only (scam unify) match)
        (test narc))

(narc-label "Match Var Dup False")

(define match-data (match '(:X :X) '(a b)))

(define msg
  "Previous data for pattern :X was 'a'; does not match new data 'b'")

(narc-expect
 (msg (if (dict? match-data)
          match-data
          (cadr match-data))))

(narc-report)
