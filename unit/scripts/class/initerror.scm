(define Trivial (make-class
                 Root
                 (n)
                 (init (v) (assign! n v))))
(Trivial (/ 1 0))
