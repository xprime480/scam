;;; Closure with dotted parameter list
;;;

(narc-label "Lambda With Dotted Formals")

(narc-expect
 ('(() (2) (2 4 #t)) (let ((f (lambda (x . y)
                                y)))
                       (list
                        (f 1)
                        (f 1 2)
                        (f 1 2 (+ 2 2) #t)))))

(narc-report)
