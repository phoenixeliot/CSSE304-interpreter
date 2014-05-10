(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
  ;; later we may add things that are not expressions.
  (eval-exp form (empty-env)))

;; eval-exp is the main component of the interpreter
(define (eval-exp exp env)
  (let ([identity-proc (lambda (x) x)])
    (cases expression exp
           ;; Theses are the core forms of the interpreter
           [lit-exp (datum) datum]
           [var-exp (id)
                    (apply-env env id 
                               identity-proc ; procedure to call if id is in the environment 
                               (lambda ()  ; procedure to call if id not in env
                                 (apply-env global-env id identity-proc
                                            (lambda () (eopl:error 'apply-env 
                                                              "variable not found in environment: ~s"
                                                              id)))))]
           [lambda-exp (re-params op-params bodies)
                       (closure re-params op-params bodies env)]
           [if-exp (condition true-body false-body)
                   (if (eval-exp condition env)
                       (eval-exp true-body env)
                       (eval-exp false-body env))]
           [app-exp (rator rands)
                    (let ([proc-value (eval-exp rator env)]
                          [args (eval-rands rands env)])
                      (apply-proc proc-value args))]
           ;; These should all be no-ops, they are simply syntax
           [and-exp (conditions)
                    (eopl:error 'eval-exp "and-exp was not expanded properly: ~s" exp)]
           [or-exp (conditions)
                   (eopl:error 'eval-exp "or-exp was not expanded properly: ~s" exp)]
           [case-exp (key patterns bodiess)
                     (eopl:error 'eval-exp "case-exp was exnot expanded properly ~s" exp)]
           [cond-exp (conditions bodiess)
                     (eopl:error 'eval-exp "cond-exp was not expanded properly ~s" exp)]
           [let-exp (type vars values bodies)
                    (eopl:error 'eval-exp "~s-exp was not expanded properly: ~s" type exp)]
           [begin-exp (bodies)
                      (eopl:error 'eval-exp "begin-exp was not expanded properly ~s" exp)]
           [while-exp (condition bodies)
                      (eopl:error 'eval-exp "while-exp was not expanded properly ~s" exp)]
           [else
            (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list
(define (eval-rands rands env)
  (map (lambda (e) (eval-exp e env)) rands))

;;  Apply a procedure to its arguments.
(define (apply-proc proc-value args)
  (cases proc-val proc-value
         [prim-proc (op) (apply-prim-proc op args)]
         [closure (re-params op-params bodies env)
                  (cond
                   [(and (not op-params) (> (length args) (length re-params)))
                    (error 'apply-proc "Too many arguments in application: ~s")]
                   [(< (length args) (length re-params))
                    (error 'apply-proc "Too few arguments in application: ~s")]
                   [else (let* ([all-params (append re-params (filter (lambda (v) v) (list op-params)))]
                                [extended-env (extend-env
                                               all-params ; symbols
                                               (encapsulate-extra-args re-params op-params args) ; values
                                               env)]) ; current environment
                           (for-each (lambda (e) (eval-exp e extended-env)) bodies))]
                   )]
         [else (error 'apply-proc
                      "Attempt to apply bad procedure: ~s" 
                      proc-value)]))

;;This puts the last items from an argument list in their own list
;;ex:
;;(encapsulate-extra-args '(a b c) 'd '(1 2 3 4 5))
;;  => (1 2 3 (4 5))
(define (encapsulate-extra-args re-params op-params args)
  (cond
   [(not op-params) args] ;don't encapsulate at all
   [(null? re-params) (list args)] ;everything leftover gets encapsulated
   [else (cons (car args)
               (encapsulate-extra-args (cdr re-params) op-params (cdr args)))]))

(define *prim-proc-names*
  '(+ - * / quotient add1 sub1 zero? not = < > <= >= apply map memv
      cons list vector null? assq eq? equal? atom? length list->vector
      list? pair? procedure? vector->list vector? make-vector vector-ref vector?
      number? symbol? set-car! set-cdr! vector-set! display newline
      car  cdr caar cddr cadr cdar caaar cdddr caadr cddar cadar cdadr cdaar caddr
      void exit))

;; Initial environment
(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc
        *prim-proc-names*)
   (empty-env)))

(define global-env init-env)

;; Procedure used for map
(define (map-proc proc ls)
  (if (null? ls)
      '()
      (cons (apply-proc proc (list (car ls))) (map-proc proc (cdr ls)))))

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define (apply-prim-proc prim-proc args)
  (case prim-proc
    [(+) (apply + args)]
    [(-) (apply - args)]
    [(*) (apply * args)]
    [(/) (apply / args)]
    [(quotient) (apply quotient args)]
    [(add1) (+ (car args) 1)]
    [(sub1) (- (car args) 1)]
    [(zero?) (= (car args) 0)]
    [(not) (apply not args)]
    [(=) (apply = args)]
    [(<) (apply < args)]
    [(>) (apply > args)]
    [(<=) (apply <= args)]
    [(>=) (apply >= args)]
    [(apply) (apply-proc (1st args) (2nd args))] 
    [(map) (map-proc (1st args) (2nd args))]
    [(memv) (apply memv args)]
    [(cons) (apply cons args)]
    [(list) args]                     ;this one is my favorite
    [(vector) (apply vector args)]
    [(null?) (apply null? args)]
    [(assq) (apply assq args)]
    [(eq?) (apply eq? args)]
    [(equal?) (apply equal? args)]
    [(atom?) (apply atom? args)]
    [(length) (apply length args)]
    [(list->vector) (apply list->vector args)]
    [(list?) (apply list? args)]
    [(pair?) (apply pair? args)]
    [(procedure?) (apply proc-val? args)]
    [(vector->list) (apply vector->list args)]
    [(vector?) (apply vector? args)]
    [(make-vector) (apply make-vector args)]
    [(vector-ref) (apply vector-ref args)]
    [(vector?) (apply vector? args)]
    [(number?) (apply number? args)]
    [(symbol?) (apply symbol? args)]
    [(set-car!) (apply set-car! args)]
    [(set-cdr!) (apply set-cdr! args)]
    [(vector-set!) (apply vector-set! args)]
    [(display) (apply display args)]
    [(newline) (apply newline args)]
    [(car) (apply car args)] 
    [(cdr) (apply cdr args)] 
    [(caar) (apply caar args)] 
    [(cddr) (apply cddr args)] 
    [(cadr) (apply cadr args)] 
    [(cdar) (apply cdar args)] 
    [(caaar) (apply caaar args)] 
    [(cdddr) (apply cdddr args)] 
    [(caadr) (apply caadr args)] 
    [(cddar) (apply cddar args)] 
    [(cadar) (apply cadar args)] 
    [(cdadr) (apply cdadr args)] 
    [(cdaar) (apply cdaar args)] 
    [(caddr) (apply caddr args)]
    [(void) (void)]
    [(exit) (exit)]
    [else (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]))

;; Check if datum is of a define datatype
(define (data-type? type datum)
  (cond
   [(expression? datum)
    (cases expression datum
           [lit-exp (datum) (eq? 'lit type)]
           [var-exp (id) (eq? 'var type)]
           [lambda-exp (re-params op-params bodies) (eq? 'lambda type)]
           [if-exp (condition true-body false-body) (eq? 'if type)]
           [let-exp (type vars values bodies) (eq? 'let type)]
           [app-exp (rator rands) (eq? 'app type)]
           [else
            #f])]
   [(proc-val? datum)
    (cases proc-val datum
           [prim-proc (name) (eq? 'prim-proc type)]
           [closure (re-params op-params bodies env) (eq? 'closure type)]
           [else
            #f])]
   [else
    #f]))

;; "read-eval-print" loop.
(define (rep)
  (display "--> ")
  ;; notice that we don't save changes to the environment...
  (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
    (cond
     [(data-type? 'closure answer)
      (set! answer '<interpreter-procedure>)]
     [(data-type? 'prim-proc answer)
      (set! answer '<primative-procedure>)])
    (eopl:pretty-print answer) (newline)
    (rep)))  ; tail-recursive, so stack doesn't grow.

;; "debug read-eval-print" loop does not sanatize values
(define (rep-debug)
  (display "--> ")
  ;; notice that we don't save changes to the environment...
  (let* ([parsed-exp (syntax-expand (parse-exp (read)))]
         [answer (top-level-eval parsed-exp)])
    (eopl:pretty-print parsed-exp) (newline)
    (eopl:pretty-print answer) (newline)
    (rep-debug)))  ; tail-recursive, so stack doesn't grow.

(define (eval-one-exp x)
  (top-level-eval (syntax-expand (parse-exp x))))








