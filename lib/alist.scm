
(load "lib/prelude.scm")

(define AssocList
  (make-class
   Root
   (*data*)

   (init ()
         (set! *data* '()))

   (length ()
           (length *data*))

   (put (key value)
        (begin
          (self remove key)
          (set! *data*
                (cons (cons key value) *data*))))

   (get (key)
        (let ((item (filter (lambda (entry)
                              (equal? key (car entry))) *data*)))
          (if (null? item)
              item
              (cdar item))))

   (remove (key)
           (set! *data*
                 (filter (lambda (item)
                           (not (equal? key (car item))))
                         *data*)))))
