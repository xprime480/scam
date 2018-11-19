
(define x 0)
(define y 0)

(if (spawn)
    (progn
     (assign! x (+ x 1))
     (assign! y (+ y 1)))
    (progn
     (assign! y (+ y 1))))

(list x y)
