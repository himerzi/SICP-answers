;;Ex 4.35 - First of Amb eval ex
(define (an-integer-between a b)
  (require (> b a))
  (amb (+ a 1) (an-integer-between (+ a 1) b)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple-between low)
  (let ((i (an-integer-starting-from low))
         (j (an-integer-starting-from low))
         (k (an-integer-starting-from low)))
    (require (>= j i))
    (require (>= k j))
    (require (= (+ (* i i) (* j j))
                (* i i k)))
    (list i j k)))

;; Ex 4.42 "liars puzzle"
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (liars)
  (let ((betty (amb 1 3))
        (ethel (amb 1 5))
        (joan (amb 3 2))
        (kitty 2)
        (mary 4))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))


(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (require p)
  (if (not p) (amb)))

;;Ex 4.49

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  
 
  (let ((found-word (amb (cadr word-list) (parse-word))))
    
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

'(sentence (noun-phrase (article the) (noun cat)) (verb eats))

(define prepositions '(prep for to in by with))
x
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
