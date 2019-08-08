;;; Test operator / on integers
;;;

(narc-label "Integer Divide")

(define check
  (lambda (val)
    (list val
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(1 #t #t #t)       (check (/)))
 ('(1/2 #t #f #t)     (check (/ 2)))
 ('(7/2 #t #f #t)     (check (/ 7 2)))
 ('(2 #t #t #t)       (check (/ 4 2)))
 ('(#i-1/3 #t #f #f)  (check (/ 2 2 -3.0))))

(narc-report)
