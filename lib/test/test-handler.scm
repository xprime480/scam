;;;(define **top** ())
;;;
;;;(call/cc
;;; (lambda (k)
;;;   (set! **top** k)))
;;;
;;;(define-syntax test-err-cat
;;;  (syntax-rules ()
;;;      ((_ form)
;;;       (with-exception-handler
;;;        (lambda (err)
;;;          (**top** (error-category err)))
;;;        (lambda ()
;;;          form)))))
;;;

#t
