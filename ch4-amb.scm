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