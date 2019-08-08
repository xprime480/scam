;;; Test operator + on complex numbers
;;;

(narc-label "Complex Plus")

(define check
  (lambda (val)
    (list val
          (complex? val)
          (real? val)
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(+i #t #f #f #f #t)        (check (+ +i)))
 ('(2+2i #t #f #f #f #t)      (check (+ 1+I 1+I)))
 ('(#i5/2+2i #t #f #f #f #f)  (check (+ 1.5+i 1+i)))
 ('(2 #t #t #t #t #t)         (check (+ 1+i 1-i)))
 ('(36/7+i #t #f #f #f #t)    (check (+ 1+i 1-i 22/7+i))))

(narc-report)
