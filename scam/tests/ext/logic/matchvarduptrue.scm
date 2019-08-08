;;; Match recurrent pattern to conforming data
;;;

(narc-label "Match Var Dup True")

(narc-expect
 ({ :X a } (match '(:X :X) '(a a))))

(narc-report)
