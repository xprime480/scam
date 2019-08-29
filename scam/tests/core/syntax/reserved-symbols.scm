(import (lib test narc))

(narc-label "Define Syntax With Reserved Symbols")

(define-syntax sym-test
  (syntax-rules (=> foo)
    ((sym-test => x xs ...) (list x x))
    ((sym-test x) x)))

(narc-expect
 (1      (sym-test 1))
 ('(1 1) (sym-test => 1 2 3)))

(narc-catch
 (:syntax (sym-test 1 2 3)))

(narc-report)
