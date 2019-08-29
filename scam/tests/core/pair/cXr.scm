(import lib/listops
        (lib test narc))

(narc-label "Car / Cdr combinations")

(define l1 '((a b) c))
(define l2 '(((x y) b) (q r) z))
(define l3 '((((x y) b) (q r) z) (((1 2) 3) foo) (4 5) 6))

(narc-expect
 ('a   (caar l1))
 ('c   (cadr l1))
 ('(b) (cdar l1))
 ('()  (cddr l1))

 ('x   (caaar l2))
 ('q   (caadr l2))
 ('b   (cadar l2))
 ('z   (caddr l2))
 ('(y) (cdaar l2))
 ('(r) (cdadr l2))
 ('()  (cddar l2))
 ('()  (cdddr l2))

 ('x     (caaaar l3))
 ('(1 2) (caaadr l3))
 ('q     (caadar l3))
 ('4     (caaddr l3))
 ('b     (cadaar l3))
 ('foo   (cadadr l3))
 ('z     (caddar l3))
 ('6     (cadddr l3))
 ('(y)   (cdaaar l3))
 ('(3)   (cdaadr l3))
 ('(r)   (cdadar l3))
 ('(5)   (cdaddr l3))
 ('()    (cddaar l3))
 ('()    (cddadr l3))
 ('()    (cdddar l3))
 ('()    (cddddr l3)))

(narc-report)
