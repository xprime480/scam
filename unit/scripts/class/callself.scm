
(define Parent (make-class
                Root
                ()
                (jarthur () (* (self doodle) 17))
		(doodle () 1)))

(define Trivial (make-class
                 Parent
                 ()
                 (get () (parent jarthur))
		 (doodle () 2)))

((Trivial) get)
