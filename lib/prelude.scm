(define max
  (lambda (x y)
    (if (> x y)
        x
        y)))

(define min
  (lambda (x y)
    (if (< x y)
        x
        y)))

(define while
  (macro (test body)
    `(letrec
         ((loop
           (lambda ()
             (if ,test
                 (progn
                   ,body
                   (loop))
                 ()))))
       (loop))))

(define map
  (lambda (fn seq)
    (if (nil? seq)
        ()
        (cons (fn (car seq)) (map fn (cdr seq))))))

(define even?
  (lambda (x)
    (and (integer? x)
         (eq? 0 (% x 2)))))

(define odd?
  (lambda (x)
    (and (integer? x)
         (not (even? x)))))

(define require
  (lambda (x)
    (if x x (amb))))

(define mem?
  (lambda (val lst)
    (if (nil? lst)
        #f
        (let ((item (car lst))
              (rest (cdr lst)))
          (or (eq? val item) (mem? val rest))))))

(define distinct?
  (lambda (lst)
    (if (nil? lst)
        #t
        (let ((item (car lst))
              (rest (cdr lst)))
          (and (not (mem? item rest))
               (distinct? rest))))))

(define xor
  (lambda (a b)
    (or (and a (not b))
        (and (not a) b))))

(define one-of
  (lambda (lst)
    (begin
      (require (and (list? lst)
                    (not (nil? lst))))
      (amb (car lst)
           (one-of (cdr lst))))))

(define exclude
  (lambda (vals lst)
    (if (nil? lst)
        lst
        (let ((item (car lst))
              (rest (cdr lst)))
          (if (mem? item vals)
              (exclude vals rest)
              (cons item (exclude vals rest)))))))

1
