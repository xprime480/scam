(load "lib/listops.scm")

(define map
  (lambda (fn seq)
    (if (null? seq)
        ()
        (cons (fn (car seq)) (map fn (cdr seq))))))

(define reduce
  (lambda (fn init seq)
    (letrec ((helper (lambda (acc tail)
                       (if (null? tail)
                           acc
                           (helper (fn acc (car tail)) (cdr tail))))))
      (helper init seq))))

(define filter
  (lambda (fn lst)
    (if (null? lst)
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
                 (begin
                   ,body
                   (loop))
                 ()))))
       (loop))))

(define old-length
  (lambda (some)
    (if (null? some)
        0
        (if (pair? some)
            (+ 1 (old-length (cdr some)))
            (if (vector? some)
                (vlen some)
                (if (dict? some)
                    (some :length)
                    (error "Cannot take length of unknown type")))))))

(define nth
  (lambda (idx some)
    (if (pair? some)
        (if (= 0 idx)
            (car some)
            (nth (- idx 1) (cdr some)))
        (if (vector? some)
            (vref idx some)
            (error "Cannot index unknown type")))))

(define even?
  (lambda (x)
    (and (integer? x)
         (= 0 (% x 2)))))

(define odd?
  (lambda (x)
    (and (integer? x)
         (not (even? x)))))

(define require
  (lambda (x)
    (if x x (amb))))

(define member?
  (lambda (val lst)
    (if (null? lst)
        #f
        (let ((item (car lst))
              (tail (cdr lst)))
          (or (equal? val item) (member? val tail))))))

(define distinct?
  (lambda (lst)
    (if (null? lst)
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
                    (not (null? lst))))
      (amb (car lst)
           (one-of (cdr lst))))))

(define cross
  (lambda (fn fst snd)
    (if (or (null? fst) (null? snd))
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
    (if (null? elts)
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
                 (not (null? item))))
          (mfn (lambda (item)
                 `(quote ,item))))
      (eval `(amb ,@(map mfn (filter ffn (power-set lst))))))))

(define exclude
  (lambda (vals lst)
    (if (null? lst)
        lst
        (let ((item (car lst))
              (tail (cdr lst)))
          (if (member? item vals)
              (exclude vals tail)
              (cons item (exclude vals tail)))))))

(define cond
  (macro (clauses)

    ;; if there are no clauses return empty list (implementation defined)
    ;;
    (if (null? clauses)
        '()
        (begin
          (let* ((clause (car clauses))
                 (test (car clause))
                 (forms (cdr clause)))

            ;; check for the "else" keyword
            ;;
            (if (equal? test 'else)
                (if (null? forms)
                    ''else
                    `(begin ,@forms))

                ;; check for a test with no forms; return test
                ;; value if truthy
                ;;
                (if (null? forms)
                    `(let ((testval ,test))
                       (if testval
                           testval
                           (cond ,(cdr clauses))))

                    ;; check for the "=>" form
                    ;;
                    (if (equal? (car forms) '=>)
                        `(let ((testval ,test))
                           (if testval
                               (apply ,@(cadr forms)
                                      (list testval))
                               (cond ,(cdr clauses))))

                        ;; normal case: if test is true, eval forms
                        ;; else recurse
                        ;;
                        `(if ,test
                             (begin ,@forms)
                             (cond ,(cdr clauses)))))))))))

(define include
  (lambda files
    (if (null? files)
        (error "include requires at least one file")
        (letrec ((include-files
                  (lambda (files)
                    (let* ((file (car files))
                           (rest (cdr files))
                           (val (load file)))
                      (if (or (error? val) (null? rest))
                          val
                          (include-files rest))))))
          (include-files files)))))

(define string-foldcase
  (lambda (str)
    (string-downcase str)))

(define substring
  (lambda (str start end)
    (string-copy str start end)))

(define type-helper=?
  (lambda (pred bs)
    (if (null? bs)
        #t
        (if (= 1 (old-length bs))
            (pred (car bs))
            (let ((fst (car bs))
                  (snd (cadr bs))
                  (rst (cdr bs)))
              (and (pred fst)
                   (eqv? fst snd)
                   (type-helper=? pred rst)))))))

(define boolean=?
  (lambda (b1 b2 . bs)
    (type-helper=? boolean? (cons b1 (cons b2 bs)))))

#t
