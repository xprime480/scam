
(define x 0)
(define y 0)

(if (spawn)
    (begin
      (set! x (+ x 1))
      (set! y (+ y 1)))
    (begin
      (set! y (+ y 1))))

(list x y)
