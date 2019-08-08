;;; Test operator - on integers
;;;

(narc-label "Integer Minus")

(define check
  (lambda (val)
    (list val (integer? val) (exact? val))))

(narc-expect
 ('(0 #t #t)   (check (-)))
 ('(-2 #t #t)  (check (- 2)))
 ('(5 #t #t)   (check (- 7 2)))
 ('(4 #t #t)   (check (- 2 2 -1 -3)))
 ('(#i4 #t #f) (check (- 2 2 -1 -3.0))))

(narc-report)
