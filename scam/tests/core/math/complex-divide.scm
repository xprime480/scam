;;; Test operator / on complex numbers
;;;

(narc-label "Complex Divide")

(define check
  (lambda (val)
    (list val
          (complex? val)
          (real? val)
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(+i #t #f #f #f #t)       (check (/ 1+i 1-i)))
 ('(#i+i #t #f #f #f #f)     (check (/ 1+i 1.0-i)))
 ('(3/5+1/5i #t #f #f #f #t) (check (/ 1+i 2+i)))
 ('(1 #t #t #t #t #t)        (check (/ 1+i 1+i))))

(narc-report)
