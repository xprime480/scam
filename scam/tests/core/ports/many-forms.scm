;;; String Port usage
;;;

(narc-label "String Port")

(define text "1 2 3 4")

(narc-expect
 ('(1 2 3 4 #t) (let ((data (open-input-string text)))
                  (list
                   (read data)
                   (read data)
                   (read data)
                   (read data)
                   (eof-object? (read data))))))

(narc-report)
