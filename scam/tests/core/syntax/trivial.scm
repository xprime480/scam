;;; define-syntax form parses
;;;

(narc-label "Define Syntax Trivial")

(define-syntax two
  (syntax-rules ()
    ((two)
     2)))

(narc-expect
 (2 (two)))

(narc-report)
