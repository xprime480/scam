;;; Test operator - on rationals
;;;

(narc-label "Rational Minus")

(define check
  (lambda (val)
    (list val
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(-5/2 #t #f #t)   (check (- 5/2)))
 ('(1/3 #t #f #t)    (check (- 2/3 1/3)))
 ('(1 #t #t #t)      (check (- 4/3 1/3)))
 ('(#i-1/3 #t #f #f) (check (- #i1/3 1/3 1/3))))

(narc-report)
