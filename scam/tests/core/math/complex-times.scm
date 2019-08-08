;;; Test operator * on complex numbers
;;;

(narc-label "Complex Times")

(define check
  (lambda (val)
    (list val
          (complex? val)
          (real? val)
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(1+i #t #f #f #f #t) (check (* 1+i)))
 ('(5-i #t #f #f #f #t) (check (* 1+i 2-3i)))
 ('(2 #t #t #t #t #t)   (check (* 1+i 1-i))))

(narc-report)
