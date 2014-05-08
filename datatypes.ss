
;; Parsed expression datatypes
(define (void? x) (equal? x (void)))

(define (lit? x)
  (lambda (x)
    (ormap 
      (lambda (pred) (pred x))
      (list number? vector? boolean? symbol? string? pair? null? void?))))

(define-datatype expression expression?
  [var-exp        ; variable references
    (id symbol?)]
  [lit-exp        ; "Normal" data
    (datum lit?)]
  [lambda-exp
    (re-params (list-of symbol?)) ;; required params
    (op-params (lambda (p) (or (eq? #f p) (symbol? p)))) ;; optional params
    (bodies (list-of expression?))]
  [if-exp
    (condition expression?)
    (true-body expression?)
    (false-body expression?)]
  [let-exp
    (vars (list-of symbol?))
    (values (list-of expression?))
    (bodies (list-of expression?))]
  [begin-exp
   (bodies (list-of expression?))]
  [and-exp
   (conditions (list-of expression?))]
  [or-exp
   (conditions (list-of expression?))]
  [cond-exp
   (conditions (list-of expression?))
   (bodiess (list-of (list-of expression?)))]
  [app-exp        ; application
    (rator expression?)
    (rands (list-of expression?))])

;; environment type definitions
(define (scheme-value? x)
  #t)

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)])

;; datatype for procedures
(define-datatype proc-val proc-val?
  [prim-proc
    (name symbol?)]
  [closure
    (re-params (lambda (p) (or (eq? #f p) (andmap symbol? p))))
    (op-params (lambda (p) (or (eq? #f p) (symbol? p))))
    (bodies (list-of expression?))
    (env environment?)])
