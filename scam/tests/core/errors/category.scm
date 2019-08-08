;;; Error categories
;;;

(narc-label "Error Category")

(define handler
  (lambda (err)
    (list
     (read-error? err)
     (file-error? err)
     (error-category err))))

(define make-read-error
  (lambda () (read (open-input-string "(where is the rest of my list"))))

(define make-file-error
  (lambda () (load "nobody-home.txt")))

(define make-user-error
  (lambda () (error "cake and %{0} ice cream" 'chocolate)))

(narc-expect
 ('(#t #f :read) (with-exception-handler handler make-read-error))
 ('(#f #t :file) (with-exception-handler handler make-file-error))
 ('(#f #f :user) (with-exception-handler handler make-user-error)))

(narc-report)
