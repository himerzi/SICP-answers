;;;;METACIRCULAR EVALUATOR THAT SEPARATES ANALYSIS FROM EXECUTION
;;;; FROM SECTION 4.1.7 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.
;;;;**WARNING: Don't load mceval twice (or you'll lose the primitives
;;;;  interface, due to renamings of apply).

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two lines at the end of the file ch4-mceval.scm
;;;; (setting up the global environment and starting the driver loop).


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.1.7 definition
;; of eval overrides the definition from 4.1.1
(load "/Users/md/CS/SICP/ch4-mceval.scm")

;;;SECTION 4.1.7

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (display (list "self evaluating:" exp)
          )
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp)
         (display (list "variable:" exp))
         (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp)
         (display (list "Definition:" exp))
         (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp)
         (display (list "lambda: " exp))
         (analyze-lambda exp))
        ((begin? exp)
         (display (list "begin: " exp))
         (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp)
         (display (list "application: " exp))
         (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      
      (execute-application (fproc env)
                           (map (lambda (aproc)
                                  (if (thunk? aproc)
                                      aproc
                                      (aproc env))) ;;don't call
                                ;;the ones that shuold be lazy - cycle
                                ;;through the defintion for that fproc
                                ;;(its in the fproc object) and check
                                ;;for operands that should be memoe'd
                                ;;and memo them.
                                ;;realize them in primitive procedure
                                ;;if they are lazy - not in compound
                       ;;procedure
                                
                                (argument-lazy-checker aprocs
                                                       (procedure-parameters (fproc env)) env))))))



(define (execute-application proc args)
  ;;(bkpt 'test-2 'd)
  (cond ((primitive-procedure? proc)
         ;;check if there are lazys and realize them
         (apply-primitive-procedure proc (map force-it args)))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameter-names proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))
(define (procedure-parameter-names proc)
  (map (lambda (param)
         (if (lazy-param? param)
             (car param)
             param))
       (procedure-parameters proc)))
(define (argument-lazy-checker args defs env)
  (cond
   ((not (pair? defs)) ;;this is for dealing with primitive procedures
    ;;where defs wont be a nice list of parameters
    args)
   ((null? defs)
    '())
   ((lazy-param? (car defs))
    (cons (build-lazy (car args) env)
          (argument-lazy-checker (cdr args) (cdr defs) env)))
   (else (cons (car args)
               (argument-lazy-checker (cdr args) (cdr defs) env)))))

(define (build-lazy exp env)
  (list exp env 'lazy))
(define (lazy-param? exp)
  (if (pair? exp)
      (eq? (cadr exp) 'lazy)
      #f))
(define (thunk? exp)
  (if (pair? exp)
      (eq? (caddr exp) 'lazy)
      #f))
(define (thunk-exp thunk) (car thunk))
(define (thunk-env thunk) (cadr thunk))
(define (force-it obj)
  ;(bkpt 'test-2 'd)
  (if (thunk? obj)
      ((thunk-exp obj) (thunk-env obj))
      obj))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print (force-it output))))
  (driver-loop))

'ANALYZING-METACIRCULAR-EVALUATOR-LOADED
