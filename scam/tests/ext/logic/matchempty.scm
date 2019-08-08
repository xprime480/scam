;;; Match empty pattern to empty data
;;;

(narc-label "Match Empty")

(define match-data (match () ()))

(narc-expect
 (#t (dict? match-data))
 (0  (match-data :length)))

(narc-report)
