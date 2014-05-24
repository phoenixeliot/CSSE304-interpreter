(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
  (eval-exp form (empty-env) (ident-k)))

(define (print . o)
  (if #f
      (begin
        (for-each display o)
        (newline))))

;; eval-exp is the main component of the interpreter
(define (eval-exp exp env k)
  (let ([identity-proc (lambda (x) x)])
    (cases expression exp
           ;; Theses are the core forms of the interpreter
           [lit-exp (datum)
                    (apply-k k datum)]
           [var-exp (id)
                    (apply-k k (apply-env-with-global id env))]
           [define-exp (id val)            
             (eval-exp val env (define-k id k))]
           [set!-exp (id val)
                     (eval-exp val env (set!-k id env k))]
           [lambda-exp (re-params op-params bodies)
                       (apply-k k (closure re-params op-params bodies env))]
           [if-exp (condition true-body false-body)
                   (eval-exp condition env (if-k true-body false-body env k))]
           [app-exp (rator rands)
                    (eval-exp rator env (rator-k rands env k))]
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

(define (apply-k k val)
  (cases continuation k
         [ident-k ()
                  val]
         [if-k (true-exp false-exp env k)
               (if val
                   (eval-exp true-exp env k)
                   (eval-exp false-exp env k))]
         [rator-k (rands env k)
                  (eval-rands rands
                              env
                              (rands-k val k))]
         [rands-k (proc-value k)
                  (apply-proc proc-value val k)]
         [map1-k (proc-cps ls k)
                 (map-cps proc-cps (cdr ls) (map2-k val k))]
         [map2-k (v1 k)
                 (apply-k k (cons v1 val))]
         [closure-app-k (k)
                        (apply-k k (car (reverse val)))]
         [set!-k (id env k)
                 (apply-k k (set-ref! (apply-env-ref-with-global id env) val))]
         [define-k (id k)
           (apply-k
            k
            (set! global-env
              (cases environment global-env
                     [empty-env-record ()
                                       (extended-env-record
                                        (list id)
                                        (list (box val))
                                        (empty-env))]
                     [extended-env-record (ids vals parent-env)
                                         (extended-env-record
                                          (cons id ids)
                                          (cons (box val) vals)
                                          (empty-env))])))])) 

;; evaluate the list of operands, putting results into a list
(define (map-cps proc-cps ls k)
  (if (null? ls)
      (apply-k k '())
      (proc-cps (car ls) (map1-k proc-cps ls k))))

(define (eval-rands rands env k)
  (map-cps (lambda (e k) (eval-exp e env k)) rands k))

;;  Apply a procedure to its arguments.
(define (apply-proc proc-value args k)
  (print "===apply-proc===")
  (print "proc-value: " proc-value)
  (print "args: " args)
  (print "k: " k "\n")
  (cases proc-val proc-value
         [prim-proc (op)
                    (print "===prim-proc===")
                    (apply-prim-proc op args k)]
         [closure (re-params op-params bodies env)
                  (let* ([all-params (append re-params (filter (lambda (v) v) (list op-params)))]
                         [extended-env (extend-env
                                        all-params ; symbols
                                        (encapsulate-extra-args re-params op-params args) ; values
                                        env)]) ; current environment
                    (map-cps (lambda (e k) (eval-exp e extended-env k)) bodies (closure-app-k k)))]
         [else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)]))

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
(define (map-proc proc ls k)
  (if (null? ls)
      '()
      (cons (apply-proc proc (list (car ls)) k) (map-proc proc (cdr ls) k))))

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define (apply-prim-proc prim-proc args k)
  (case prim-proc
    [(+) (apply-k k (apply + args))]
    [(-) (apply-k k (apply - args))]
    [(*) (apply-k k (apply * args))]
    [(/) (apply-k k (apply / args))]
    [(quotient) (apply-k k (apply quotient args))]
    [(add1) (apply-k k (+ (car args) 1))]
    [(sub1) (apply-k k (- (car args) 1))]
    [(zero?) (apply-k k (= (car args) 0))]
    [(not) (apply-k k (apply not args))]
    [(=) (apply-k k (apply = args))]
    [(<) (apply-k k (apply < args))]
    [(>) (apply-k k (apply > args))]
    [(<=) (apply-k k (apply <= args))]
    [(>=) (apply-k k (apply >= args))]
    [(apply) (apply-k k (apply-proc (1st args) (2nd args) k))] 
    [(map) (apply-k k (map-proc (1st args) (2nd args) k))]
    [(memv) (apply-k k (apply memv args))]
    [(cons) (apply-k k (apply cons args))]
    [(list) (apply-k k args)]      
    [(vector) (apply-k k (apply vector args))]
    [(null?) (apply-k k (apply null? args))]
    [(assq) (apply-k k (apply assq args))]
    [(eq?) (apply-k k (apply eq? args))]
    [(eqv?) (apply-k k (apply eqv? args))]
    [(equal?) (apply-k k (apply equal? args))]
    [(atom?) (apply-k k (apply atom? args))]
    [(length) (apply-k k (apply length args))]
    [(list->vector) (apply-k k (apply list->vector args))]
    [(list?) (apply-k k (apply list? args))]
    [(pair?) (apply-k k (apply pair? args))]
    [(procedure?) (apply-k k (apply proc-val? args))]
    [(vector->list) (apply-k k (apply vector->list args))]
    [(vector?) (apply-k k (apply vector? args))]
    [(make-vector) (apply-k k (apply make-vector args))]
    [(vector-ref) (apply-k k (apply vector-ref args))]
    [(vector?) (apply-k k (apply vector? args))]
    [(number?) (apply-k k (apply number? args))]
    [(symbol?) (apply-k k (apply symbol? args))]
    [(set-car!) (apply-k k (apply set-car! args))]
    [(set-cdr!) (apply-k k (apply set-cdr! args))]
    [(vector-set!) (apply-k k (apply vector-set! args))]
    [(display) (apply-k k (apply display args))]
    [(newline) (apply-k k (apply newline args))]
    [(list-tail) (apply-k k (apply list-tail args))]
    [(car) (apply-k k (apply car args))] 
    [(cdr) (apply-k k (apply cdr args))] 
    [(caar) (apply-k k (apply caar args))] 
    [(cddr) (apply-k k (apply cddr args))] 
    [(cadr) (apply-k k (apply cadr args))] 
    [(cdar) (apply-k k (apply cdar args))] 
    [(caaar) (apply-k k (apply caaar args))] 
    [(cdddr) (apply-k k (apply cdddr args))] 
    [(caadr) (apply-k k (apply caadr args))] 
    [(cddar) (apply-k k (apply cddar args))] 
    [(cadar) (apply-k k (apply cadar args))] 
    [(cdadr) (apply-k k (apply cdadr args))] 
    [(cdaar) (apply-k k (apply cdaar args))] 
    [(caddr) (apply-k k (apply caddr args))]
    [(void) (apply-k k (void))]
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








