;;; Match recurrent pattern to mismatching data
;;;

(narc-label "Match Var Dup False")

(load "lib/listops.scm")

(define match-data (match '(:X :X) '(a b)))

(define msg
  "Previous data for pattern :X was 'a'; does not match new data 'b'")

(narc-expect
 (msg (if (dict? match-data)
          match-data
          (cadr match-data))))

(narc-report)
