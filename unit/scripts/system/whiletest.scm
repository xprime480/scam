
(define x 5)
(define y 0)

(while (> x 0)
       (progn
        (assign! y (+ x y))
        (assign! x (- x 1))))

y

