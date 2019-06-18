
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define caaar (lambda (x) (car (caar x))))
(define caadr (lambda (x) (car (cadr x))))
(define cadar (lambda (x) (car (cdar x))))
(define caddr (lambda (x) (car (cddr x))))
(define cdaar (lambda (x) (cdr (caar x))))
(define cdadr (lambda (x) (cdr (cadr x))))
(define cddar (lambda (x) (cdr (cdar x))))
(define cdddr (lambda (x) (cdr (cddr x))))

(define caaaar (lambda (x) (car (caaar x))))
(define caaadr (lambda (x) (car (caadr x))))
(define caadar (lambda (x) (car (cadar x))))
(define caaddr (lambda (x) (car (caddr x))))
(define cadaar (lambda (x) (car (cdaar x))))
(define cadadr (lambda (x) (car (cdadr x))))
(define caddar (lambda (x) (car (cddar x))))
(define cadddr (lambda (x) (car (cdddr x))))
(define cdaaar (lambda (x) (cdr (caaar x))))
(define cdaadr (lambda (x) (cdr (caadr x))))
(define cdadar (lambda (x) (cdr (cadar x))))
(define cdaddr (lambda (x) (cdr (caddr x))))
(define cddaar (lambda (x) (cdr (cdaar x))))
(define cddadr (lambda (x) (cdr (cdadr x))))
(define cdddar (lambda (x) (cdr (cddar x))))
(define cddddr (lambda (x) (cdr (cdddr x))))

(define length
  (lambda (lst)
    (letrec ((list-length
              (lambda (l)
                (if (null? l)
                    0
                    (+ 1 (list-length (cdr l)))))))
      (if (list? lst)
          (list-length lst)
          (error "length: expected a list, got: %{0}" lst)))))

(define reverse
  (lambda (lst)
    (letrec ((list-reverse
              (lambda (l)
                (if (null? l)
                    l
                    (append (reverse (cdr l)) (list (car l)))))))
      (if (list? lst)
          (list-reverse lst)
          (error "reverse expected a list, got: %{0}" lst)))))

#t
