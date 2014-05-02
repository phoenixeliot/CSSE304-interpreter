;; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.
;; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4rd caddr)

(define (parse-exp datum)
  (cond
   [(symbol? datum) (var-exp datum)]
   [(void? datum) (lit-exp datum)]
   [(number? datum) (lit-exp datum)]
   [(boolean? datum) (lit-exp datum)]
   [(string? datum) (lit-exp datum)]
   [(vector? datum) (lit-exp datum)]
   [(equal? 'quote (1st datum)) (lit-exp (2nd datum))]
   [(pair? datum)
    (cond
     [(equal? 'if (1st datum))
                                        ;(valid-if-exp? datum) ;this will error if not valid
      (if (equal? 3 (length datum))
          (if-exp
           (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (void))) ; One armed if
          (apply if-exp (map parse-exp (cdr datum)))) ;normal if
      ]
     [(equal? 'let (1st datum))
      (valid-let-exp? datum) ;; error checking
      (let-exp 
       (map 1st (2nd datum)) ; don't parse the variable names, following 'lambda style
       (map parse-exp (map 2nd (2nd datum))) ; values of variable names
       (map parse-exp (cddr datum))) ; bodies of let
      ]
     [else ; application
      (app-exp (parse-exp (1st datum))  ; rator
               (map parse-exp (cdr datum))) ; rand
      ])]
   [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))

;; Error checking functions
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
    (eopl:error 'parse-exp "vars in ~s-expression must be symbols: ~s" (car datum) datum))
   (else #t)))




