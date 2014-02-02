#lang racket

;; ex 1.42
(define (compose f g)
  (lambda (x)
    (f(g x))))

;; ex 1.43
(define (repeated fn n)
  (lambda (x)
    (define (repeated-r n acc)
      (if (= n 0)
          acc
          (repeated-r (- n 1) (fn acc))))
    (repeated-r n x)))

;; ex 1.43 - using compose (ex 1.42) - tail recursive - non tail recursive impelmentation also possible
(define (repeated-compose fn n)
  (define (repeated-r n acc)
      (if (= n 1)
          acc
          (repeated-r (- n 1) (compose acc acc))))
    (repeated-r n fn))