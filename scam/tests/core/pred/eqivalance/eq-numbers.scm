(import (lib test narc))

(narc-label "Eq for numbers")

(define test
  (lambda (n1 n2 n3 n4)
    (list (eq? n1 n1)
          (eq? n1 n2)
          (eq? n1 n3)
          (eq? n3 n4)
          (eq? n4 n4))))

(narc-expect
 ('(#t #f #f #f #t) (let ((n1 0)
                          (n2 0)
                          (n3 0.0)
                          (n4 0.0))
                      (test n1 n2 n3 n4)))

 ('(#t #f #f #f #t) (let ((n1 1.5)
                          (n2 1.5)
                          (n3 3/2)
                          (n4 3/2))
                      (test n1 n2 n3 n4)))

 ('(#t #f #f #f #t) (let ((n1 1+i)
                          (n2 1+i)
                          (n3 -i)
                          (n4 -i))
                      (test n1 n2 n3 n4)))

 ('(#t #t #f) (list (eq? -inf.0 -inf.0)
                    (eq? +inf.0 +inf.0)
                    (eq? +nan.0 +nan.0))))

(narc-report)
