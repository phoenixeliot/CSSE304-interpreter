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
           [else
            (*eopl:error* "this shouldn't happen")]))