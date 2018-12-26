(define integers-from
  (lambda (x)
    (amb x
         (integers-from (+ x 1)))))

(define integers-between
  (lambda (lower upper)
    (begin
      (require (<= lower upper))
      (amb lower
           (integers-between (+ lower 1) upper)))))

(define square
  (lambda (n)
    (* n n)))

(define sum
  (lambda (lst)
    (apply + lst)))
