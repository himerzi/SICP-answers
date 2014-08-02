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
