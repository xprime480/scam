;;; Test operator - on complex numbers
;;;

(narc-label "Complex Minus")

(define check
  (lambda (val)
    (list val
          (complex? val)
          (real? val)
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(-i #t #f #f #f #t)        (check (- +i)))
 ('(1-2i #t #f #f #f #t)      (check (- 2+I 1+3I)))
 ('(#i3/2+2i #t #f #f #f #f)  (check (- 1.5+i -i)))
 ('(-1 #t #t #t #t #t)        (check (- 1+i 2+i)))
 ('(-15/7+i #t #f #f #f #t)   (check (- 2+i 1-i 22/7+i))))

(narc-report)
