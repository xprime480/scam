(define max
  (lambda (x y)
    (if (> x y)
        x
        y)))

(define min
  (lambda (x y)
    (if (< x y)
        x
        y)))

(define while
  (macro (test body)
    `(letrec
         ((loop
           (lambda ()
             (if ,test
                 (progn
                   ,body
                   (loop))
                 ()))))
       (loop))))

(define map
  (lambda (fn seq)
    (if (nil? seq)
        ()
        (cons (fn (car seq)) (map fn (cdr seq))))))

1
