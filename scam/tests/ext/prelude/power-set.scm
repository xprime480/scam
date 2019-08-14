(import lib/prelude)

(narc-label "Power-Set")

(narc-expect
 ('(())     (power-set '()))
 ('(() (1)) (power-set '(1)))

 ('(4 #t #t #t #t)
  (let ((pset (power-set '(1 2))))
    (list
     (length pset)
     (member? '() pset)
     (member? '(1) pset)
     (member? '(2) pset)
     (member? '(1 2) pset)))))

(narc-report)
