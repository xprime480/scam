(import (test narc))

(narc-label "Define Syntax With Arguments")

(define-syntax foo
  (syntax-rules ()
    ((foo a)
     (list a a))
    ((foo)
     "nada")))

(narc-expect
 ('("nada" (1 1)) (list (foo) (foo 1))))

(narc-report)
