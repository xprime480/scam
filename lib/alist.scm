
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
        (progn
         (self remove key)
         (assign! *data*
                  (cons (cons key value) *data*))))

   (get (key)
        (let ((item (filter (lambda (entry)
                              (eq? key (car entry))) *data*)))
          (if (nil? item)
            item
            (cdr (car item)))))

   (remove (key)
           (assign! *data*
                    (filter (lambda (item)
                              (not (eq? key (car item))))
                            *data*)))))
