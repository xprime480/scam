(define max (lambda (a b)
	      (if (> a b) a b)))

(define Maxxer (make-class
		Root
		(val)
		(init (v) (assign! val v))
		(get (n) (max val n))))


(define obj (Maxxer 0))

(list (obj get -42)
      (obj get (/ 84 2)))
