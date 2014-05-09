(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (syntax-expand parsed-exp)
  (cases expression parsed-exp
         [lit-exp (datum) (lit-exp datum)]
         [var-exp (id) (var-exp id)]
         [lambda-exp (re-params op-params bodies)
                     (lambda-exp re-params op-params (map syntax-expand bodies))]
         [if-exp (condition true-body false-body)
                 (if-exp (syntax-expand condition)
                         (syntax-expand true-body)
                         (syntax-expand false-body))]
         [let-exp (type vars values bodies)
                  (cond
                   [(eq? type 'let)
                    (app-exp (lambda-exp vars #f (map syntax-expand bodies))
                             (map syntax-expand values))]
                   [(eq? type 'let*)
                    (syntax-expand
                     (if (null? vars)
                         (begin-exp bodies)
                         (let-exp 'let
                                  (list (1st vars))
                                  (list (1st values))
                                  (list (syntax-expand
                                         (let-exp 'let* (cdr vars) (cdr values) bodies))))))]
                   [else
                    (eopl:error 'syntax-expand "Invalid let type ~s" parsed-exp)]
                   )]
         [app-exp (rator rands) (app-exp (syntax-expand rator) (map syntax-expand rands))]
         [begin-exp (bodies) (app-exp (lambda-exp '() #f (map syntax-expand bodies)) '())]
         [and-exp (conditions)
                  (cond
                   [(null? conditions)
                    (lit-exp #t)]
                   [(null? (cdr conditions))
                    (1st conditions)]
                   [else
                    (if-exp (1st conditions)
                            (syntax-expand (and-exp (cdr conditions)))
                            (lit-exp #f))])]
         [or-exp (conditions)
                 ;; value-name is a unique name to record the value of the current condition
                 (let ([value-name '_:_or-temp_:_]) 
                   (cond
                    [(null? conditions)
                     (lit-exp #f)]
                    [(null? (cdr conditions))
                     (1st conditions)]
                    [else
                     (syntax-expand (let-exp 'let
                                     (list value-name)
                                     (list (1st conditions))
                                     (list (if-exp (var-exp value-name)
                                                   (var-exp value-name)
                                                   (or-exp (cdr conditions))))))]))]
         [case-exp (key patterns bodiess)
                   (let ([value-name '_:_case-temp_:_]) 
                     (syntax-expand
                      (let-exp 'let
                       (list value-name)
                       (list key)
                       (list (cond-exp ; convert to cond
                              (map (lambda (pattern)
                                     (if (equal? pattern '(else))
                                         (var-exp 'else)
                                         (app-exp
                                          (var-exp 'memv)
                                          (list (var-exp value-name)
                                                (lit-exp pattern)))))
                                   patterns)
                              bodiess)))))]
         [cond-exp (conditions bodiess) ;list of bodies
                   (if (null? conditions)
                       (app-exp (var-exp 'void) '())
                       (if (equal? (var-exp 'else) (1st conditions))
                           (syntax-expand (begin-exp (1st bodiess)))
                           (if-exp (syntax-expand (1st conditions))
                                   (syntax-expand (begin-exp (map syntax-expand (1st bodiess))))
                                   (syntax-expand (cond-exp (cdr conditions) (cdr bodiess))))))]
         [while-exp (condition bodies)
            (let ((loop-name '_:_loop_:_))
              (syntax-expand (let-exp 'let
                       (list loop-name)
                       (list (lambda-exp (list loop-name)
                                         #f
                                         (list (if-exp condition
                                            (begin-exp (append bodies
                                                              (list (app-exp (var-exp loop-name)
                                                                             (list (var-exp loop-name))))))
                                            (app-exp (var-exp 'void) '())))))
                       (list (app-exp (var-exp loop-name) (list (var-exp loop-name))))
                     ))
            )
         ]
         [else
          (eopl:error 'syntax-expand "Unhandled parsed-exp: ~s" parsed-exp)]))
