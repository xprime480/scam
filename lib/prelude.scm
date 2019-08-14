(import lib/listops)

(define (map fn seq)
  (if (null? seq)
      ()
      (cons (fn (car seq)) (map fn (cdr seq)))))

(define (reduce fn init seq)
  (letrec ((helper (lambda (acc tail)
                     (if (null? tail)
                         acc
                         (helper (fn acc (car tail)) (cdr tail))))))
    (helper init seq)))

(define (filter fn lst)
  (if (null? lst)
      lst
      (let ((head (car lst))
            (tail (cdr lst)))
        (if (fn head)
            (cons head (filter fn tail))
            (filter fn tail)))))

(define (max x y)
  (if (> x y)
      x
      y))

(define (min x y)
  (if (< x y)
      x
      y))

(define-syntax while
  (syntax-rules ()
    ((while2 test body)
     (letrec
         ((loop (lambda ()
                  (if test
                      (begin
                        body
                        (loop))
                      ()))))
       (loop)))))

(define (nth idx some)
  (if (pair? some)
      (if (= 0 idx)
          (car some)
          (nth (- idx 1) (cdr some)))
      (if (vector? some)
          (vref idx some)
          (error :args "Cannot index unknown type"))))

(define (even? x)
  (and (integer? x)
       (= 0 (% x 2))))

(define (odd? x)
  (and (integer? x)
       (not (even? x))))

(define (require x)
  (if x x (amb)))

(define (member? val lst)
  (if (null? lst)
      #f
      (let ((item (car lst))
            (tail (cdr lst)))
        (or (equal? val item) (member? val tail)))))

(define (distinct? lst)
  (if (null? lst)
      #t
      (let ((item (car lst))
            (tail (cdr lst)))
        (and (not (member? item tail))
             (distinct? tail)))))

(define (xor a b)
  (or (and a (not b))
      (and (not a) b)))

(define (one-of lst)
  (begin
    (require (and (list? lst)
                  (not (null? lst))))
    (amb (car lst)
         (one-of (cdr lst)))))

(define (cross fn fst snd)
  (if (or (null? fst) (null? snd))
      '()
      (reduce append
              '()
              (map (lambda (fitem)
                     (map (lambda (sitem)
                            (fn fitem sitem))
                          snd))
                   fst))))

(define (power-set elts)
  (if (null? elts)
      (list '())
      (let ((first (list (car elts)))
            (pscdr (power-set (cdr elts))))
        (append pscdr
                (cross (lambda (item elts) (cons item elts))
                       first
                       pscdr)))))

(define (some-of lst)
  (let ((ffn (lambda (item)
               (not (null? item))))
        (mfn (lambda (item)
               `(quote ,item))))
    (eval `(amb ,@(map mfn (filter ffn (power-set lst))))
          (interaction-environment))))

(define (exclude vals lst)
  (if (null? lst)
      lst
      (let ((item (car lst))
            (tail (cdr lst)))
        (if (member? item vals)
            (exclude vals tail)
            (cons item (exclude vals tail))))))

;;; taken directly from the R7RS specification p. 68
;;
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define (include . files)
    (if (null? files)
        (error "include requires at least one file")
        (letrec ((include-files
                  (lambda (files)
                    (let* ((file (car files))
                           (rest (cdr files))
                           (val (load file)))
                      (if (or (error-object? val) (null? rest))
                          val
                          (include-files rest))))))
          (include-files files))))

(define (string-foldcase str)
  (string-downcase str))

(define (substring str start end)
  (string-copy str start end))

(define (type-helper=? pred bs)
  (if (null? bs)
      #t
      (if (= 1 (length bs))
          (pred (car bs))
          (let ((fst (car bs))
                (snd (cadr bs))
                (rst (cdr bs)))
            (and (pred fst)
                 (eqv? fst snd)
                 (type-helper=? pred rst))))))

(define (boolean=? b1 b2 . bs)
  (type-helper=? boolean? (cons b1 (cons b2 bs))))

#t
