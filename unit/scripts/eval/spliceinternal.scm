(define x 99)
`(+ x ,@(list 3 (+ x 1)) 2)
