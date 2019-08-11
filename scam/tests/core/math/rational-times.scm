;;; Test operator * on rationals
;;;

(narc-label "Rational Times")

(define check
  (lambda (val)
    (list val
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(5/2 #t #f #t)    (check (* 5/2)))
 ('(2/9 #t #f #t)    (check (* 2/3 1/3)))
 ('(1 #t #t #t)      (check (* 4/3 3/4)))
 ('(#i1/27 #t #f #f) (check (* #i1/3 1/3 1/3))))

(narc-report)