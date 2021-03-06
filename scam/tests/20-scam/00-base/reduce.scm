(import (only (scheme base) + append list)
        (only (scam base) reduce)
        (test narc))

(narc-label "Reduce")

(define (local-sum a b)
  (+ a b))

(define (appender l1 l2)
  (append l1 (list l2)))

(narc-expect
 (0  (reduce local-sum 0 '()))
 (5  (reduce local-sum 0 '(5)))
 (15 (reduce local-sum 0 '(1 2 3 4 5)))
 (5  (reduce local-sum -10 '(1 2 3 4 5)))

 ('(#f 1 3 2)
  (reduce append '(#f) '((1 3) (2))))

 ('((#t) (1 3) (2))
  (reduce appender '((#t)) '((1 3) (2)))))

(narc-report)
