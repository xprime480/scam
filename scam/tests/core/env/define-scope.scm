;;; Define in an inner scope does not affect outer scope
;;;

(narc-label "Define Scope")

(define x 17)

(narc-expect
 ('(42 0) (let ()
            (define x 42)
            (define y 0)
            (list x y)))
 (17 x))

(narc-catch
 (:eval y))

(narc-report)
