;;; Closure with symbol for parameter list
;;;

(narc-label "Lambda With Symbol Formals")

(narc-expect
 ('(() (5) (5 10 15)) (let ((f (lambda x x)))
                        (list
                         (f)
                         (f 5)
                         (f 5 10 15)))))

(narc-report)
