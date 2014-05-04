;; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
  ;; later we may add things that are not expressions.
  (eval-exp form (empty-env)))

;; eval-exp is the main component of the interpreter
(define (eval-exp exp env)
  (let ([identity-proc (lambda (x) x)])
    (cases expression exp
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
           [let-exp (vars values bodies)
                    (let ([extended-env (extend-env
                                         vars ; symbols
                                         (map (lambda (e) (eval-exp e env)) values) ; values
                                         env)]) ;; current environment
                      (for-each (lambda (e) (eval-exp e extended-env)) bodies))]
           [app-exp (rator rands)
                    (let ([proc-value (eval-exp rator env)]
                          [args (eval-rands rands env)])
                      (apply-proc proc-value args))]
           [else
            (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list
(define (eval-rands rands env)
  (map (lambda (e) (eval-exp e env)) rands))

;;  Apply a procedure to its arguments.
;;  At this point, we only have primitive procedures.  
;;  User-defined procedures will be added later.
(define (apply-proc proc-value args)
  (cases proc-val proc-value
         [prim-proc (op) (apply-prim-proc op args)]
         [closure (re-params op-params bodies env)
                  (let ([extended-env (extend-env
                                       re-params ; symbols TODO: add optional case
                                       args ; values
                                       env)]) ;; current environment
                    (for-each (lambda (e) (eval-exp e extended-env)) bodies))]
                                        ; You will add other cases
         [else (error 'apply-proc
                      "Attempt to apply bad procedure: ~s" 
                      proc-value)]))

(define *prim-proc-names*
  '(+ - * / add1 sub1 zero? not = < > <= >=
      cons list null? assq eq? equal? atom? length list->vector
      list? pair? procedure? vector->list vector? make-vector vector-ref vector?
      number? symbol? set-car! set-cdr! vector-set! display newline
      car  cdr caar cddr cadr cdar caaar cdddr caadr cddar cadar cdadr cdaar caddr))

;; Initial environment
(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))

(define global-env init-env)

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define (apply-prim-proc prim-proc args)
  (case prim-proc
    [(+) (apply + args)]
    [(-) (apply - args)]
    [(*) (apply * args)]
    [(/) (apply / args)]
    [(add1) (apply add1 args)]
    [(sub1) (apply sub1 args)]
    [(zero?) (apply zero? args)]
    [(not) (apply not args)]
    [(=) (apply = args)]
    [(<) (apply < args)]
    [(>) (apply > args)]
    [(<=) (apply <= args)]
    [(>=) (apply >= args)]
    [(cons) (apply cons args)]
    [(list) (apply list args)]
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
    [else (error 'apply-prim-proc 
                 "Bad primitive procedure name: ~s" 
                 prim-op)]))

;; "read-eval-print" loop.
(define (rep)
  (display "--> ")
  ;; notice that we don't save changes to the environment...
  (let ([answer (top-level-eval (parse-exp (read)))])
    ;; TODO: are there answers that should display differently?
    (eopl:pretty-print answer) (newline)
    (rep)))  ; tail-recursive, so stack doesn't grow.

(define (eval-one-exp x)
  (top-level-eval (parse-exp x)))
