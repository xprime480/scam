
(define-library
    (lib2 0 1)
  (begin
    (define y (lambda () 4))
    (define x (lambda () (y)))))
