;;; Match non-empty pattern to empty data
;;;

(narc-label "Match Var False")

(load "lib/listops.scm")

(define match-data (match '(:X) '()))

(define msg "Pattern: (:X) does not conform to data: ()")

(define result (if (dict? match-data)
                   match-data
                   (cadr match-data)))

(narc-expect
 (msg result))

(narc-report)
