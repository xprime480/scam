;;; Test operator + on integers
;;;

(narc-label "Integer Plus")

(define check
  (lambda (val)
    (list val (integer? val) (exact? val))))

(narc-expect
 ('(0 #t #t)    (check (+)))
 ('(2 #t #t)    (check (+ 2)))
 ('(4 #t #t)    (check (+ 2 2)))
 ('(4 #t #t)    (check (+ 2 2 -1 -3 4)))
 ('(4.0 #t #f)  (check (+ 2 2 -1 -3 4.0))))

(narc-report)
