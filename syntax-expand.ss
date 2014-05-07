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
          [let-exp (vars values bodies)
                   (app-exp (lambda-exp vars #f (map syntax-expand bodies))
                            (map syntax-expand values))]
          [app-exp (rator rands) (app-exp (syntax-expand rator) (map syntax-expand rands))]
          [begin-exp (bodies) (app-exp (lambda-exp '() #f (map syntax-expand bodies)) '())]
          [cond-exp (conditions bodiess) ;list of bodies
            (if (null? conditions)
              (app-exp (var-exp 'void) '())
              (if (equal? (var-exp 'else) (1st conditions))
                (syntax-expand (begin-exp (1st bodiess)))
                (if-exp (syntax-expand (1st conditions))
                  (syntax-expand (begin-exp (map syntax-expand (1st bodiess))))
                  (syntax-expand (cond-exp (cdr conditions) (cdr bodiess))))))]
          [else
           (eopl:error 'syntax-expand "Unhandled parsed-exp: ~s" parsed-exp)]))