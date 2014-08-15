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

;; Exercise 4.6

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp) )
(define (let->combination exp)
  (eval-let (let-bindings exp) '() '() (let-body exp)))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (eval-let bindings vars exps body)
  (let* ((bndg (car bindings))
         (rest (cdr bindings))
         (vars (cons (car bndg)  vars))
         (exps (cons (cadr bndg) exps)))
    (if (null? rest)
        (cons (make-lambda vars body) exps)
        (eval-let rest vars exps  body))))

;; Ex 4.7
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (let ((body (let-body exp)))

    (define (build-let bindings)
      (let ((rest (cdr bindings))
            (current-bindings (list (car bindings))))
        (if (null? rest)
            (make-let current-bindings  body)
            (list 'let current-bindings (build-let rest)))))
    
    (build-let (let-bindings exp))))



;;Ex 4.define

;;my abstraction for frame traversing only

(define (frame-traverser frame val)
  (define (scan-no-recur vars vals)
    (cond ((null? vars)
           '())
          ((eq? var (car vars))
           vals)
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values frame)))

(define (set-variable-value var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
          (error "Unbound variable or something such:" var))
    (let ((result (frame-traverser (first-frame env) val)))
      (cond ((null? result)
             (env-loop (enclosing-environment env)))
            (else
             (set-car! result val))))))


;; Exercise 4.16
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Trying to access unassigned value")
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
;;B
(define (scan-out-defines body)
  (let* ((parsed (scan body '()))
         (bindings (car parsed))
         (body (cadr parsed))
         (let-exp (make-let (make-vals-unassigned bindings) '()))
         (set!-exps (make-set!-exps bindings)))
    let-exp
    ;;(append  set!-exps body)
    ))

(define (make-set!-exps bindings)
  (map (lambda (binding)
         (list 'set! (car binding) (cadr binding))) bindings))
(define (make-vals-unassigned vals)
  (map (lambda (el)
         (cons (car el) (list '*unassigned*))) vals))
(define (scan exps bindings)
    (let ((exp (car exps))
          (rest (cdr exps)))
      (if (definition? exp)
          (scan rest
                (cons (list (definition-variable exp) (definition-value exp)) bindings))
          (list bindings (cons exp rest)))))
(define (test)
  (define a 1)
  (define b 2)
  (define c (+ a b))
  (display c)
  (set! a 4)
  (display c))

;;Ex 4.21 a. Y comb for fibonacci number
((lambda (n)
   ((lambda  (fact)
      (fact fact n))
    (lambda (ft k)
      (cond
       ((= k 0) 0)
       ((= k 1) 1)
       (else (+ (ft ft (- k 1)) (ft ft (- k 2))))))))
 10)

;; Ex 4.22

(define (analyze-let exp)
  (lambda (env) (eval-analyze-let (let-bindings exp '() '() (let-body exp)))))

(define (eval-analyze-let bindings vars exps body)
  (let* ((bndg (car bindings))
         (rest (cdr bindings))
         (vars (cons (car bndg)  vars))
         (exp (cadr bndg))
         (exps (cons exp exps)))
    (if (null? rest)
        (analyze (cons (make-lambda vars body) exps))
        (eval-let rest vars exps body))))

;; Ex 4.31 - Implementing lazyness and memoized-lazyness as an
;; upward-compatible extention to Scheme (what)
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ;;lazyness stuff
        ((is-lazy-arg? exp)
         (delay-it exp env))
        ((is-lazy-memo-arg? exp)
         (delay-it exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure
                                    (list-of-arg-values arguments))) ;;lazy stuff
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
            (procedure-parameters procedure)
            
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (is-lazy-arg? exp)
  (tagged-list? (cdr exp) 'lazy))

(define (is-lazy-memo-arg? exp)
  (tagged-list? (cdr exp) 'lazy-memo))

(define (list-of-arg-values exps env)
  (cond  ((no-operands? exps)
          '())
         (is)
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (actual-value exp env)
  (force-it (eval exp env)))


;;Dispatch on force it based on memo or no memo -
;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env memo?)
  (if (is-lazy-memo-arg? exp)
      (list 'thunk-memo exp env)
      (list 'thunk exp env)))

;;Ex 4.31 done with analyzing evaluator
