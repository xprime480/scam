(narc-label "Car / Cdr")

(define pair (cons 1 2))
(define not-a-pair 2)
(define list-val (list 1 2 3))

(narc-expect
 (1  (car pair))
 (2  (cdr pair))

 (1       (car list-val))
 ('(2 3)  (cdr list-val))
 (2       (car (cdr list-val))))

(narc-catch
 (:args (car not-a-pair))
 (:args (cdr not-a-pair)))

(narc-report)
