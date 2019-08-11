;;; Natural language processing example from Bill Hails.
;;;

(narc-label "Parse")

(load "lib/prelude.scm")

(define verbs '(verb flies like))
(define nouns '(noun time fruit flies bannanna arrow))
(define determinants '(det a an))
(define adjectives '(adj time fruit))
(define prepositions '(prep like))

(define *unparsed* ())

(define parse
  (lambda (input)
    (begin
      (set! *unparsed* input)
      (let ((sentence (parse-sentence)))
        (begin
          (require (null? *unparsed*))
          sentence)))))

(define parse-word
  (lambda (words)
    (begin
      (require (pair? *unparsed*))
      (require (member? (car *unparsed*) (cdr words)))
      (let ((found-word (car *unparsed*)))
        (begin
          (set! *unparsed* (cdr *unparsed*))
          (list (car words) found-word))))))

(define parse-sentence
  (lambda ()
    (list 'sentence
          (amb (parse-word nouns)
               (parse-noun-phrase))
          (parse-verb-phrase))))

(define parse-verb-phrase
  (lambda ()
    (list 'verb-phrase
          (parse-word verbs)
          (amb (parse-prep-phrase)
               (parse-noun-phrase)))))

(define parse-noun-phrase
  (lambda ()
    (list 'noun-phrase
          (amb (parse-word adjectives)
               (parse-word determinants))
          (parse-word nouns))))

(define parse-prep-phrase
  (lambda ()
    (list 'prep-phrase
          (parse-word prepositions)
          (parse-noun-phrase))))

(define parse-result1
  '(sentence (noun time)
             (verb-phrase (verb flies)
                          (prep-phrase (prep like)
                                       (noun-phrase (det an)
                                                    (noun arrow))))))

(define parse-result2
  '(sentence (noun-phrase (adj time)
                          (noun flies))
             (verb-phrase (verb like)
                          (noun-phrase (det an)
                                       (noun arrow)))))

(define expected (let ((p (open-output-string)))
		   (display parse-result1 p)
		   (display " " p)
		   (display parse-result2 p)
		   (display " " p)
		   (get-output-string p)))

(define port (open-output-string))
(define foo (lambda (x)
              (display x port)
              (display " " port)))

(foo (parse '(time flies like an arrow)))
(backtrack)

(narc-catch
 (:values (backtrack)))

(narc-expect
 (expected (get-output-string port)))

(narc-report)
