
(define **narc-test-name** "")
(define **narc-test-count** 0)
(define **narc-test-pass** 0)

(define narc-label
  (lambda (name)
    (set! **narc-test-name** name)))

(define narc-expect
  (macro forms))

(define narc-catch
  (macro forms))

(define narc-report
  (lambda ()
    (let ((pass (= **narc-test-count** **narc-test-pass**)))
      (display (if pass "[PASS]" "[FAIL]"))
      (display " ")
      (display **narc-test-name**)
      (newline)
      pass)))

0
