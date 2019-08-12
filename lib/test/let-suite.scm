;;; helper functions for let tests
;;;

(define-syntax let-suite-expect
  (syntax-rules ()
    ((let-suite-expect (let-value lstar-value lrec-value) bindings forms ...)
     (narc-expect
      (let-value   (let    bindings forms ...))
      (lstar-value (let*   bindings forms ...))
      (lrec-value  (letrec bindings forms ...))))

    ((let-suite-expect all bindings forms ...)
     (let-suite-expect (all all all) bindings forms ...))))
