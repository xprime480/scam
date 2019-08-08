;;; define-syntax recognized reserved symbols in patterns
;;;

(narc-label "Define Syntax With Reserved Symbols")

(load "lib/test/test-handler.scm")

(define-syntax sym-test
  (syntax-rules (=> foo)
    ((sym-test => x xs ...) (list x x))
    ((sym-test x) x)))

(narc-expect
 (1        (sym-test 1))
 ('(1 1)   (sym-test => 1 2 3))
 (:syntax  (test-err-cat (sym-test 1 2 3))))

(narc-report)
