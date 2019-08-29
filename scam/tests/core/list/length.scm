(import lib/listops
        (lib test narc))

(narc-label "List Length")

(narc-expect
 (0  (length ()))
 (3  (length '(1 (sublist counts as 1) 3)))
 (#t (exact? (length '()))))

(narc-catch
 (:args (length))
 (:args (length () ()))
 (:args (length '(a . b))))

(narc-report)
