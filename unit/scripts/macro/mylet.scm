(define map
  (lambda (fn lst)
    (if (nil? lst)
        ()
        (cons (fn (car lst)) (map fn (cdr lst))))))

(define cars
  (lambda (lst)
    (map car lst)))

(define cadrs
  (lambda (lst)
    (map (lambda (item) (car (cdr item))) lst)))

(let* ((mylet
        (macro (bindings body)
          (let* ((names (cars bindings))
                 (values (cadrs bindings)))
            (cons (list (quote lambda) names body) values)))))
  (mylet ((a 1)
          (b 2))
         (list a b)))
