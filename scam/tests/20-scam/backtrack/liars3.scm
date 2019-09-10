;;; Liars problem from Bill Hails
;;;
;;; See liars.scm for description
;;;

(import (only (scheme base) = car list)
        (scheme eval)
        (only (scam base) xor)
        (only (scam backtrack) amb exclude require)
        (test narc))

(narc-label "Liars Optimized")

(define (liar3)
  (let* ((local-env (environment '(only (scam backtrack) amb)))
         (betty (amb 1 2 3 4 5))
         (ethel (eval `(amb ,@(exclude (list betty)
                                       (list 1 2 3 4 5)))
                      local-env))
         (joan  (eval `(amb ,@(exclude (list betty ethel)
                                       (list 1 2 3 4 5)))
                      local-env))
         (kitty (eval `(amb ,@(exclude (list betty ethel joan)
                                       (list 1 2 3 4 5)))
                      local-env))
         (mary  (car (exclude (list betty ethel joan kitty)
                              (list 1 2 3 4 5)))))
    (begin
      (require (xor (= kitty 2)
                    (= betty 3)))
      (require (xor (= ethel 1)
                    (= joan 2)))
      (require (xor (= joan 3)
                    (= ethel 5)))
      (require (xor (= kitty 2)
                    (= mary 4)))
      (require (xor (= mary 4)
                    (= betty 1)))
      `((betty ,betty)
        (ethel ,ethel)
        (joan ,joan)
        (kitty ,kitty)
        (mary ,mary)))))

(narc-expect
 ('((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
  (liar3)))

(narc-report)
