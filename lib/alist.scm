
(load "lib/prelude.scm")

(define AssocList
  (make-class
   Root
   (*data*)

   (init ()
         (assign! *data* '()))

   (length ()
           (length *data*))

   (put (key value)
        (begin
          (self remove key)
          (assign! *data*
                   (cons (cons key value) *data*))))

   (get (key)
        (let ((item (filter (lambda (entry)
                              (equal? key (car entry))) *data*)))
          (if (null? item)
              item
              (cdar item))))

   (remove (key)
           (assign! *data*
                    (filter (lambda (item)
                              (not (equal? key (car item))))
                            *data*)))))
