
(load "lib/listops.scm")

(define cars
  (lambda (lst)
    (map car lst)))

(define cadrs
  (lambda (lst)
    (map cadr lst)))

(let* ((mylet
        (macro (bindings body)
          (let* ((names (cars bindings))
                 (values (cadrs bindings)))
            (cons (list (quote lambda) names body) values)))))
  (mylet ((a 1)
          (b 2))
         (list a b)))
