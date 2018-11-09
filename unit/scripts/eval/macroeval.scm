(define var 5)
(define test (macro (x)
               `(+ ,x 2)))
(test var)
