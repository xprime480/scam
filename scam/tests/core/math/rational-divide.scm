;;; Test operator / on rationals
;;;

(narc-label "Rational Divide")

(define check
  (lambda (val)
    (list val
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(21/110 #t #f #t)  (check (/ 3/10 11/7)))
 ('(#i3/2 #t #f #f)   (check (/ 1.5 1)))
 ('(1 #t #t #t)       (check (/ 2/3 4/6)))
 ('(#i9/8 #t #f #f)   (check (/ 1/2 #i2/3 2/3))))

(narc-report)
