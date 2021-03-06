;;; Natural language processing example from Bill Hails.
;;;

(import (only (scheme base) car cdr list null? pair?)
        (only (scam base) member?)
        (only (scam backtrack) amb backtrack require)
        (test value-helper)
        (test narc))

(narc-label "Parse")

(define verbs '(verb flies like))
(define nouns '(noun time fruit flies bannanna arrow))
(define determinants '(det a an))
(define adjectives '(adj time fruit))
(define prepositions '(prep like))

(define *unparsed* ())

(define (parse input)
  (begin
    (set! *unparsed* input)
    (let ((sentence (parse-sentence)))
      (begin
        (require (null? *unparsed*))
        sentence))))

(define (parse-word words)
  (begin
    (require (pair? *unparsed*))
    (require (member? (car *unparsed*) (cdr words)))
    (let ((found-word (car *unparsed*)))
      (begin
        (set! *unparsed* (cdr *unparsed*))
        (list (car words) found-word)))))

(define (parse-sentence)
  (list 'sentence
        (amb (parse-word nouns)
             (parse-noun-phrase))
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (list 'verb-phrase
        (parse-word verbs)
        (amb (parse-prep-phrase)
             (parse-noun-phrase))))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (amb (parse-word adjectives)
             (parse-word determinants))
        (parse-word nouns)))

(define (parse-prep-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

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

(define expected
  (let ((p (ValueHelper)))
    (p update parse-result1)
    (p update parse-result2)
    (p get)))

(define helper (ValueHelper))

(helper update (parse '(time flies like an arrow)))
(backtrack)

(narc-catch
 (:values (backtrack)))

(narc-expect
 (expected (helper get)))

(narc-report)
