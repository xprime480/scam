(import (test narc))

(narc-label "Define Syntax With Literals")

(define-syntax lit-test
    (syntax-rules ()
      ((lit-test :kw "blue" #\x #t 42)
       "So Long and Thanks for All The Fish")
      ((lit-test a b c d e)
       "Failure!")))

(narc-expect
 ("So Long and Thanks for All The Fish"
  (lit-test :kw "blue" #\x #t 42))
 ("Failure!"
  (lit-test :rong "blue" #\x #t 42))
 ("Failure!"
  (lit-test :kw "green" #\x #t 42))
 ("Failure!"
  (lit-test :kw "blue" #\e #t 42))
 ("Failure!"
  (lit-test :kw "blue" #\x #f 42))
 ("Failure!"
  (lit-test :kw "blue" #\x #t 17)))

(narc-report)
