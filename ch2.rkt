#lang racket
;;my own implementation of pairs
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