
(define-library
    (lib5 0 1)
  (begin
    (define y (lambda () 4))
    (define x (lambda () (y)))))

(import (lib5))

(x)

