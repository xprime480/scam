(define SimpleConstant (make-class
                 Root
                 (val)
                 (init (v) (assign! val v))
                 (get () val)))
(define k 21)

(define answer (SimpleConstant (* k 2)))

(answer get)
