;;; Test operator * on integers
;;;

(narc-label "Integer Times")

(define check
  (lambda (val)
    (list val (integer? val) (exact? val))))

(narc-expect
 ('(1 #t #t)     (check (*)))
 ('(2 #t #t)     (check (* 2)))
 ('(6 #t #t)     (check (* 2 3)))
 ('(48 #t #t)    (check (* 2 2 -1 -3 4)))
 ('(0 #t #t)     (check (* 2 2 -1 0 4)))
 ('(48.0 #t #f)  (check (* 2 2 -1 -3 4.0))))

(narc-report)
