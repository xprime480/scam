(define map
  (lambda (fn seq)
    (if (nil? seq)
        ()
        (cons (fn (car seq)) (map fn (cdr seq))))))

(define reduce
  (lambda (fn init seq)
    (letrec ((helper (lambda (acc tail)
                       (if (nil? tail)
                           acc
                           (helper (fn acc (car tail)) (cdr tail))))))
      (helper init seq))))

(define filter
  (lambda (fn lst)
    (if (nil? lst)
        lst
        (let ((head (car lst))
              (tail (cdr lst)))
          (if (fn head)
              (cons head (filter fn tail))
              (filter fn tail))))))

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

(define length
  (lambda (lst)
    (if (nil? lst)
        0
        (+ 1 (length (cdr lst))))))

(define append
  (lambda (fst snd)
    (if (nil? fst)
        snd
        (if (= 1 (length fst))
            (cons (car fst) snd)
            (cons (car fst) (append (cdr fst) snd))))))

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

(define member?
  (lambda (val lst)
    (if (nil? lst)
        #f
        (let ((item (car lst))
              (tail (cdr lst)))
          (or (eq? val item) (member? val tail))))))

(define distinct?
  (lambda (lst)
    (if (nil? lst)
        #t
        (let ((item (car lst))
              (tail (cdr lst)))
          (and (not (member? item tail))
               (distinct? tail))))))

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

(define cross
  (lambda (fn fst snd)
    (if (or (nil? fst) (nil? snd))
        '()
        (reduce append
                '()
                (map (lambda (fitem)
                       (map (lambda (sitem)
                              (fn fitem sitem))
                            snd))
                     fst)))))

(define power-set
  (lambda (elts)
    (if (nil? elts)
        (list '())
        (let ((first (list (car elts)))
              (pscdr (power-set (cdr elts))))
          (append pscdr
                  (cross (lambda (item elts) (cons item elts))
                         first
                         pscdr))))))

(define some-of
  (lambda (lst)
    (let ((ffn (lambda (item)
                 (not (nil? item))))
          (mfn (lambda (item)
                 `(quote ,item))))
      (eval `(amb ,@(map mfn (filter ffn (power-set lst))))))))

(define exclude
  (lambda (vals lst)
    (if (nil? lst)
        lst
        (let ((item (car lst))
              (tail (cdr lst)))
          (if (member? item vals)
              (exclude vals tail)
              (cons item (exclude vals tail)))))))

1
