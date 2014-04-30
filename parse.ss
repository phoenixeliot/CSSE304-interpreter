; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(void? datum) (lit-exp datum)]
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(boolean? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(equal? 'quote (1st datum)) (lit-exp datum)]
      [(pair? datum)
        (cond
          [(equal? 'if (1st datum))
            ;(valid-if-exp? datum) ;this will error if not valid
            (if (equal? 3 (length datum))
              (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (void)))
              (apply if-exp (map parse-exp (cdr datum))))
          ]
          [(equal? 'let (1st datum))
            (valid-let-exp? datum)
            (let-exp 
              (map car (cadr datum)) ; don't parse the variable names, following 'lambda style
              (map parse-exp (map cadr (cadr datum)))
              (map parse-exp (cddr datum)))
          ]
          [else (app-exp (parse-exp (1st datum))
             (map parse-exp (cdr datum)))])]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (valid-let-exp? datum)
  (cond
    ((< (length datum) 3)
      (eopl:error 'parse-exp "~s expression: incorrect length: ~s" (car datum) datum))
    ((not (list? (cadr datum)))
      (eopl:error 'parse-exp "declarations in ~s-expression is not a list" (car datum) datum))
    ((not (andmap list? (cadr datum)))
      (eopl:error 'parse-exp "declaration in ~s-expression is not a list" (car datum) datum))
    ((not (andmap (lambda (ls) (= 2 (length ls))) (cadr datum)))
      (eopl:error 'parse-exp "declaration in ~s-expression must be a list of length 2: ~s" (car datum) datum))
    ((not (andmap (lambda (ls) (symbol? (car ls))) (cadr datum)))
      (eopl:error 'parse-exp "vars in ~s-expression must be symbols: ~s" (car datum) datum)))
  (else #t))




