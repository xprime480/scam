(import lib/prelude)

(narc-label "While")

(narc-expect
 (15   (begin
         (define x 5)
         (define y 0)
         (while (> x 0)
                (begin
                  (set! y (+ x y))
                  (set! x (- x 1))))

         y)))

(narc-report)