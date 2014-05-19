(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
  (eval-exp form (empty-env)))

;; eval-exp is the main component of the interpreter
(define (eval-exp exp env)
  (let ([identity-proc (lambda (x) x)])
    (cases expression exp
           ;; Theses are the core forms of the interpreter
           [lit-exp (datum) datum]
           [var-exp (id)
                    (let ([val (apply-env-with-global id env)])
                      (if (box? val) ; id passed by reference?
                          (unbox val)
                          val))]
           [define-exp (id val)
             (apply-env-ref global-env id
                            (lambda (ref)
                              (set-ref! ref (eval-exp val env)))
                            (lambda ()
                              (set! global-env
                                (cases environment global-env
                                       [empty-env-record
                                        ()
                                        (extended-env-record
                                         (list id) (list (box (eval-exp val env))) (empty-env))]
                                       [extended-env-record
                                        (ids vals parent-env)
                                        (extended-env-record
                                         (cons id ids) (cons (box (eval-exp val env)) vals) (empty-env))]))))]
           [set!-exp (id val)
                     (let ([ref (apply-env-ref-with-global id env)])
                       (set-ref!
                        (if (box? (unbox ref)) ; id passed by reference?
                            (unbox ref)
                            ref)
                        (eval-exp val env)))]
           [ref-exp (id)
                    id]
           [lambda-exp (re-params op-params bodies)
                       (closure re-params op-params bodies env)]
           [ref-lambda-exp (params bodies)
                           (ref-closure params bodies env)]
           [if-exp (condition true-body false-body)
                   (if (eval-exp condition env)
                       (eval-exp true-body env)
                       (eval-exp false-body env))]
           [app-exp (rator rands)
                    (let* ([proc-value (eval-exp rator env)]
                           [args (if (data-type? 'ref-closure proc-value)
                                     (map (lambda (a p) (if (symbol? p) ; TODO: use representation independent solution
                                                       (eval-exp a env)
                                                       (apply-env-ref-with-global (2nd a) env)))
                                          rands (2nd proc-value))
                                     (eval-rands rands env))])
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
         [ref-closure (params bodies env)
                      (let ([extended-env (extend-env
                                           (map (lambda  (p) (if (symbol? p) ; TODO: use representation independent solution
                                                            p
                                                            (2nd p)))
                                                params) ; symbols
                                           args ; values
                                           env)])
                        (for-each (lambda (e) (eval-exp e extended-env)) bodies))]
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
      cons list vector null? assq eq? eqv? equal? atom? length list->vector
      list? pair? procedure? vector->list vector? make-vector vector-ref vector?
      number? symbol? set-car! set-cdr! vector-set! display newline list-tail
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

(define (reset-global-env)
  (set! global-env init-env))

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
    [(eqv?) (apply eqv? args)]
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
    [(list-tail) (apply list-tail args)]
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
    [else (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]))

;; "read-eval-print" loop.
(define (rep)
  (display "--> ")
  (let ([read (read)])
    (if (not (equal? read '(exit)))
        (let ([answer (top-level-eval (syntax-expand (parse-exp read)))])
          (cond
           [(data-type? 'closure answer)
            (set! answer '<interpreter-procedure>)]
           [(data-type? 'prim-proc answer)
            (set! answer '<primative-procedure>)])
          (if (not (eq? answer (void)))        
              (eopl:pretty-print answer) (newline))
          (rep)))))  ; tail-recursive, so stack doesn't grow.

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








