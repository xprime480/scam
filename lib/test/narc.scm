
(define **narc-test-name** "")
(define **narc-test-count** 0)
(define **narc-test-pass** 0)

(define narc-label
  (lambda (name)
    (set! **narc-test-name** name)))

(define-syntax narc-expect
  (syntax-rules ()
    ((narc-expect (value form) ...)
     (begin
       (let* ((temp form)
	      (pass (equal? value temp)))
         (set! **narc-test-count** (+ **narc-test-count** 1))
         (if pass
             (set! **narc-test-pass** (+ **narc-test-pass** 1))
	     (begin
	       (display "Expected: <")
	       (display value)
	       (display "> Got: <")
	       (display temp)
	       (display ">")
	       (newline)
	       #f))
         pass) ...))))

(define-syntax narc-catch
  (syntax-rules ()
    ((narc-catch (value form) ...)
     (narc-expect (value (with-exception-handler
			  (lambda (err)
			    (error-category err))
			  (lambda ()
			    form)))) ... )))

(define narc-report
  (lambda ()
    (let ((pass (equal? **narc-test-count** **narc-test-pass**)))
      (display (if pass "[PASS]" "[FAIL]"))
      (display " ")
      (display **narc-test-name**)
      (newline)
      pass)))

0
