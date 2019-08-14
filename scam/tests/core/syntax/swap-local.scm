(narc-label "Define Syntax For Swap")

(define (foo)
  (let ((x 1) (y 2))
    (define-syntax swap!
      (syntax-rules ()
        ((swap! a b)
         (let ((tmp a))
           (set! a b)
           (set! b tmp)))))
    (swap! x y)
    (list x y)))

(narc-expect
 ('(2 1) (foo)))

(narc-report)
