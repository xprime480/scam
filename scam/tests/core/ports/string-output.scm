;;; String Output Port usage
;;;

(narc-label "String Output Port")

(narc-expect
 ("1 2 3" (let ((sink (open-output-string)))
            (display 1 sink)
            (display " " sink)
            (display 2 sink)
            (display " " sink)
            (display 3 sink)
            (get-output-string sink))))

(narc-report)
