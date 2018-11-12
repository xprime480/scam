(define Trivial (make-class
                 Root
                 ()
                 (get () (self jarthur 1))
		 (jarthur (n) (* n 17))))
((Trivial) get)
