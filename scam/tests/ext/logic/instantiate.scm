;;; Test the instantiate function
;;;

(narc-label "Instantiate")

(load "lib/prelude.scm")

(narc-expect
 ('() (instantiate '()))

 ("test" (instantiate "test"))

 (#t (let ((result (instantiate :X)))
       (and (keyword? result)
            (not (equal? :X result)))))

 (#t (let ((result (instantiate '(:Q))))
       (and (pair? result)
            (let ((r2 (car result)))
              (and (keyword? r2)
                   (not (equal? :Q r2)))))))

 (#t (let ((result (instantiate '(:Q :Q))))
       (and (pair? result)
            (let ((r2 (car result))
                  (r3 (cadr result)))
              (and (keyword? r2)
                   (keyword? r3)
                   (equal? r2 r3)
                   (not (equal? :Q r2)))))))

 (#t (let ((result (instantiate '(:Q :E :Q))))
       (and (pair? result)
            (let ((r2 (car result))
                  (r3 (cadr result))
                  (r4 (caddr result)))
              (and (keyword? r2)
                   (keyword? r3)
                   (keyword? r4)
                   (equal? r2 r4)
                   (not (equal? r2 r3))
                   (not (equal? :Q r2))
                   (not (equal? :E r3)))))))

 (#t (let ((result (instantiate #(7 :123))))
       (and (vector? result)
            (equal? 2 (vlen result))
            (let ((r2 (nth 0 result))
                  (r3 (nth 1 result)))
              (and (equal? r2 7)
                   (keyword? r3)
                   (not (equal? :123 r3)))))))

 (#t (let ((result (instantiate { :k1 :v1 :k2 7 :k3 :v1 } )))
       (and (dict? result)
            (equal? 3 (result :length))
            (let ((r2 (result :get :k1))
                  (r3 (result :get :k2))
                  (r4 (result :get :k3)))
              (and (equal? r3 7)
                   (keyword? r2)
                   (keyword? r4)
                   (not (equal? :v1 r2))
                   (equal? r2 r4))))))

 (#t (let ((r1 (instantiate :X))
           (r2 (instantiate :X)))
       (and (keyword? r1)
            (keyword? r2)
            (not (equal? r1 :X))
            (not (equal? r2 :X))
            (not (equal? r1 r2))))))

(narc-report)
