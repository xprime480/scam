
(define **narc-test-name** "")
(define **narc-test-count** 0)
(define **narc-test-pass** 0)

(define narc-label
  (lambda (name)
    (set! **narc-test-name** name)))

(define-syntax narc-expect
  (syntax-rules
      ((narc-expect value forms ...)
       (let ((temp (begin forms ...)))
         (equal? value temp)))))

(define-syntax narc-catch
  (syntax-rules
      ((narc-catch form forms ...)
       (let ((temp (begin form forms ...)))
         temp))))

(define narc-report
  (lambda ()
    (let ((pass (= **narc-test-count** **narc-test-pass**)))
      (display (if pass "[PASS]" "[FAIL]"))
      (display " ")
      (display **narc-test-name**)
      (newline)
      pass)))

0
