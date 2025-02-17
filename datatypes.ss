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
  [ref-lambda-exp
   (params (list-of
            (lambda (v) (or (symbol? v) (data-type? 'ref v)))))
   (bodies (list-of expression?))]
  [if-exp
   (condition expression?)
   (true-body expression?)
   (false-body expression?)]
  [define-exp
    (id symbol?)
    (val expression?)]
  [set!-exp
   (id symbol?)
   (val expression?)]
  [ref-exp
   (id symbol?)]
  [let-exp ; let, let*
   (type symbol?)
   (vars (list-of symbol?))
   (values (list-of expression?))
   (bodies (list-of expression?))]
  [begin-exp
   (bodies (list-of expression?))]
  [and-exp
   (conditions (list-of expression?))]
  [or-exp
   (conditions (list-of expression?))]
  [case-exp
   (key expression?)
   (patterns (list-of list?))
   (bodiess (list-of (list-of expression?)))]
  [cond-exp
   (conditions (list-of expression?))
   (bodiess (list-of (list-of expression?)))]
  [while-exp
   (condition expression?)
   (bodies (list-of expression?))]
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
   (re-params (list-of symbol?))
   (op-params (lambda (p) (or (eq? #f p) (symbol? p))))
   (bodies (list-of expression?))
   (env environment?)]
  [ref-closure
   (params (list-of (lambda (v) (or (symbol? v) (data-type? 'ref v)))))
   (bodies (list-of expression?))
   (env environment?)])

;; continuation datatype
(define-datatype continuation continuation?
  [identity-k]
  [if-k
   (true-exp expression?)
   (false-exp expression?)
   (env environment?)
   (k continuation?)]
  [rator-k
   (rands (list-of? expression?))
   (env environment?)
   (k continuation?)]
  [rands-k
   (proc-value scheme-value?)
   (k continuation?)])

(define (apply-k k val)
  (cases continuation k
         [identity-k () val]
         [if-k (true-exp false-exp env k)
               (if val
                   (eval-exp true-exp env k)
                   (eval-exp false-exp env k))]
         [rator-k (rands env k)
                  (eval-rands rands
                              env
                              (rands-k val k))]
         [rands-k (proc-value k)
                  (apply-proc proc-value val k)]))

;; Check if datum is of a define datatype
(define (data-type? type datum)
  (cond
   [(expression? datum)
    (cases expression datum
           [lit-exp (datum) (eq? 'lit type)]
           [var-exp (id) (eq? 'var type)]
           [lambda-exp (re-params op-params bodies) (eq? 'lambda type)]
           [ref-lambda-exp (params bodies) (eq? 'ref-lambda type)]
           [if-exp (condition true-body false-body) (eq? 'if type)]
           [define-exp (id val) (eq? 'define type)]
           [set!-exp (id val)  (eq? 'set! type)]
           [ref-exp (id) (eq? 'ref type)]
           [let-exp (type vars values bodies) (eq? 'let type)]
           [begin-exp (bodies) (eq? 'begin type)]
           [and-exp (conditions) (eq? 'and type)]
           [or-exp (conditions) (eq? 'or type)]
           [case-exp (key patterns bodiess) (eq? 'case type)]
           [cond-exp (conditions bodiess) (eq? 'cond type)]
           [while-exp (condition bodies) (eq? 'while type)]
           [app-exp (rator rands) (eq? 'app type)]
           [else
            #f])]
   [(proc-val? datum)
    (cases proc-val datum
           [prim-proc (name) (eq? 'prim-proc type)]
           [closure (re-params op-params bodies env) (eq? 'closure type)]
           [ref-closure (params bodies env) (eq? 'ref-closure type)]
           [else
            #f])]
   [else #f]))
