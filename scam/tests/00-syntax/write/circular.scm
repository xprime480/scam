(import (scheme base)
        (scheme write)
        (test narc))

(narc-label "Write Circular List")

(define s1 (open-output-string))
(define x (cons 1 2))
(set-cdr! x x)
(display x s1)

(define s2 (open-output-string))
(define q (cons 1 2))
(set-car! q q)
(display q s2)

(narc-expect
 ("#0=(1 . #0#)" (get-output-string s1))
 ("#0=(#0# . 2)" (get-output-string s2)))

(narc-report)
