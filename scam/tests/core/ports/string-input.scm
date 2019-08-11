;;; String Input Port usage
;;;

(narc-label "String Input Port")

(narc-expect
 ('(#t (+ 1 1)) (let ((data (open-input-string "(+ 1 1)")))
                  (list
                   (port? data)
                   (read data)))))

(narc-report)
