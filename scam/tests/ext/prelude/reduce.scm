(narc-label "Reduce")

(load "lib/prelude.scm")

(define (local-sum a b)
  (+ a b))

(narc-expect
 (0  (reduce local-sum 0 '()))
 (5  (reduce local-sum 0 '(5)))
 (15 (reduce local-sum 0 '(1 2 3 4 5)))
 (5  (reduce local-sum -10 '(1 2 3 4 5)))

 ('(#f 1 3 2) (reduce append '(#f) '((1 3) (2))))

 ('((#t) (1 3) (2))
  (let ((f (lambda (l1 l2)
             (append l1 (list l2)))))
    (reduce f '((#t)) '((1 3) (2))))))

(narc-report)
