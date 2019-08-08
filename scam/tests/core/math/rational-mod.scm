;;; Test operator % on rationals
;;;

(narc-label "Rational Modulus")

(define check
  (lambda (val)
    (list val
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(0.0 #t #t #f)   (check (% 12.5 2.5)))
 ('(#i1/4 #t #f #f) (check (% 12.75 0.5)))
 ('(1/3 #t #f #t)   (check (% 5/3 2/3))))

(narc-report)
