;;; Test the eqv? function on composite types (lists, vectors, ...)
;;;

(narc-label "Eqv for Composites")


(define test
  (lambda (t1 t2 t3)
    (list (eqv? t1 t1)
          (eqv? t1 t2)
          (eqv? t1 t3))))

(narc-expect
 ('(#t #f #f) (let ((test1 "bunny")
                    (test2 "bunny")
                    (test3 "carlos"))
                (test test1 test2 test3)))

 ('(#t #f #f) (let ((test1 '(1 2 3))
                    (test2 '(1 2 3))
                    (test3 '(5 4 3)))
                (test test1 test2 test3)))

 ('(#t #f #f) (let ((test1 #(1 2 3))
                    (test2 #(1 2 3))
                    (test3 #(5 4 3)))
                (test test1 test2 test3)))

 ('(#t #f #f) (let ((test1 #u8(1 2 3))
                    (test2 #u8(1 2 3))
                    (test3 #u8(5 4 3)))
                (test test1 test2 test3)))

 ('(#t #f #f) (let ((test1 { :a 1 :b 2 })
                    (test2 { :a 1 :b 2 })
                    (test3 { :x 99 :u 98 }))
                (test test1 test2 test3))))

(narc-report)
