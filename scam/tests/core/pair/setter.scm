;;; Test modification of components of pair values
;;;

(narc-label "Set-Car / Set-Cdr")

(load "lib/test/test-handler.scm")

(define immutable '(1 . 2))
(define mutable (cons 1 2))
(define list-val (list 1 2 3))

(narc-expect
 (:args (test-err-cat (set-car! immutable 3)))
 (:args  (test-err-cat (set-cdr! immutable 4)))

 ('(3 . 2)   (begin
               (set-car! mutable 3)
               mutable))

 ('(3 5 5 5) (begin
               (set-cdr! mutable '(5 5 5))
               mutable))

 ('(3 2 3)   (begin
               (set-car! list-val 3)
               list-val))

 ('(3)       (begin
               (set-cdr! list-val '())
               list-val)))

(narc-report)
