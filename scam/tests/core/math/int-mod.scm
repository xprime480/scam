;;; Test operator % on integers
;;;

(narc-label "Integer Modulus")

(define check
  (lambda (val)
    (list val
          (rational? val)
          (integer? val)
          (exact? val))))

(narc-expect
 ('(0 #t #t #t)   (check (%)))
 ('(0 #t #t #t)   (check (% 2)))
 ('(1 #t #t #t)   (check (% 7 2)))
 ('(1.0 #t #t #f) (check (% 7 2.0)))
 ('(33 #t #t #t)  (check (% 77 44 0 0))))  ;; this is an error in proper scheme!

(narc-report)
