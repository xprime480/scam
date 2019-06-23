(define **top** ())

(call/cc
 (lambda (k)
   (set! **top** k)))

(define handler
  (lambda (err)
    (**top** (error-category err))))

(define test-err-cat
  (macro (form)
    `(with-exception-handler
      handler
      (lambda () ,form))))
