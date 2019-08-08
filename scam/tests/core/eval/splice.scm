;;; Quasiquote with splicing
;;;

(narc-label "Splice")

(define x 99)

(narc-expect
 ('(+ x 3 100 2) `(+ x ,@(list 3 (+ x 1)) 2))
 ('(3 100)       `(,@(list 3 (+ x 1))))
 ('(+ x 2)       `(+ x ,@(list) 2)))

(narc-report)
