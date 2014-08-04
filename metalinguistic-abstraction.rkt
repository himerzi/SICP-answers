;Ex 4.1
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
                                        ;LR

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let (left)
        (let ((list-of-values (rest-operands exps) env))))) )

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

                                        ;Ex 4.3
(define (eval exp env)
  ;;self evaluation, vars and application are special cases
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ((get 'eval (car exp)) (cdr exp) env)
   ((application? exp)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env))))
  (define (definition exp env) (eval-definition exp env))
  ( (get 'eval (car exp)) (cdr exp) env)
  (put 'eval 'define definition))

;;Ex 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (and-op1 exp) (car exp))
(define (and-op2 exp) (cdr exp))
(define (and->if exp env)
  (eval (eval-and (cdr exp)) env))

(define (eval-and exp)
  (let ((first (and-op1 exp))
        (rest (and-op2 exp)))
    (if (null? rest)
        (make-if first
                 first
                 'false)
        (make-if first
                 (eval-and rest)
                 'false))))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-op1 exp) (car exp))
(define (or-rest exp) (cdr exp))
(define (or->if exp env)
  (eval (eval-or (cdr exp))))
(define (eval-or exp)
  (let ((first (or-op1 exp))
        (rest (or-rest exp)))
    (if (null? first)
        'false        
        (make-if first
                 first
                 (eval-or rest)))))

;;Ex 4.5

(define (cond-=>-clause?  clause)
  (eq? (cadr clause) '=>))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                                         ; 
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (cond-=>-clause? first)
                (let ((predicate-val (cond-predicate first)))
                  (make-if predicate-val
                           (list  (cdr (cond-actions first)) predicate-val)
                           (expand-clauses rest)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

