;;; define-syntax accepts rest symbol
;;;

(narc-label "Define Syntax With Rest Formal")

(define-syntax rest-test
  (syntax-rules ()
    ((rest-test . vars)
     'vars)))

(narc-expect
 ('(1 2 3) (rest-test 1 2 3)))

(narc-report)
