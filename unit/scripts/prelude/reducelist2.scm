
(let ((f (lambda (l1 l2)
	   (append l1 (list l2)))))
  (reduce f '((#t)) '((1 3) (2))))
