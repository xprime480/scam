
(define-library
    (lib1 0 1)
  (export x)
  (begin
    (define y (lambda () 4))
    (define x (lambda () (y)))))
