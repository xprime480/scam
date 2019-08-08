;;; Define symbol twice
;;;

(narc-label "Symbol Defined Twice")

(narc-catch
 (:env (begin
         (define x 1)
         (define x 2)
	 x)))

(narc-report)
