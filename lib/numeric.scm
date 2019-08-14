(define (integers-from x)
  (amb x
       (integers-from (+ x 1))))

(define (integers-between lower upper)
  (require (<= lower upper))
  (amb lower
       (integers-between (+ lower 1) upper)))

(define (square n) 
  (* n n))

(define (sum lst) 
  (apply + lst))
