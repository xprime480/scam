
(narc-label "Reverse Lists")

(import lib/listops)

(narc-expect
 ('()           (reverse '()))
 ('(3 2 1)      (reverse '(1 2 3)))
 ('(4 (2 3) 1)  (reverse '(1 (2 3) 4))))

(narc-catch
 (:args (reverse))
 (:args (reverse '() '(a b c))))

(narc-report)
