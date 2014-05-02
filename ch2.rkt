#lang racket
;;my own implementation of pairs (see 2.1.3)
(define (m-cons a b)
  (lambda (which)
    (cond ((= 1 which)
           a)
          ((= 2 which)
           b)
          (else  "Error"))))

(define (m-car x)
  (x 1))
(define (m-cdr x)
  (x 2))

;;Ex 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))
(last-pair (list 23 72 149 34))

;;Exercise 2.18: Define a procedure reverse that takes a list as
;;argument and returns a list of the same elements in reverse order:
(define (reverse l)
  (define (iter forward reverse)
    (if (null? forward)
        reverse
        (iter (cdr forward)  (cons (car forward) reverse))))
  (iter l (list)))
(reverse (list 1 2 3 4 5))

;;

(define us-coins (list 1 10 25 5 50))

(define (count-change amount)
  (cc amount us-coins))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? kinds-of-coins)) 0)
        (else (+ ( cc amount
                      (except-first-denomination kinds-of-coins))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (car kinds-of-coins))
(define (except-first-denomination kinds-of-coins)
  (cdr kinds-of-coins))
(define (no-more? kinds-of-coins)
  (null? kinds-of-coins))
(first-denomination 3)
(no-more? '(2))

(count-change 11)