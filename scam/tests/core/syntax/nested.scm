;;; define-syntax validates nested patterns
;;;

(narc-label "Define Syntax With Nested Pattern")

(define-syntax nest-test
  (syntax-rules ()
    ((nest-test (a (b c)))
     b)))

(narc-expect
 (2 (nest-test (1 (2 3)))))

(narc-report)
