
(define x 2)

(define Trivial (let ((x 3))
                  (make-class
                   Root
                   (q)
                   (init (n) (assign! q n))
                   (get (n) (list q x n)))))

(define t (Trivial x))

(let ((x 5))
  (t get x))
