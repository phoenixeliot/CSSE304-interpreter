(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.
;; Procedures to make the parser a little bit saner.
(define (parse-lambda-args p)
  (cond
   [(symbol? p) (values '() p)] ; No re-params
   [(null? p) (values p #f)] ; No op-parmas or re-params
   [else
    (let ([res (let loop ([p p] [res '()])
                 (cond
                  [(symbol? (cdr p))
                   (list (cons (car p) res) (cdr p))]
                  [(null? (cdr p))
                   (list (cons (car p) res) #f)]
                  [else
                   (loop (cdr p) (cons (car p) res))]))])
      (values (reverse (car res)) (cadr res)))]))

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
     [(eqv? 'lambda (1st datum))
      ;;(valid-lambda? datum)
      (let-values ([(re-params op-params)
                    (parse-lambda-args (2nd datum))])
        (lambda-exp re-params
                    op-params
                    (map parse-exp (cddr datum))))
      ]
     [(eqv? 'if (1st datum))
      ;;(valid-if? datum)
      (if (equal? 3 (length datum))
          (if-exp
           (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (void))) ; One armed if
          (apply if-exp (map parse-exp (cdr datum)))) ;normal if
      ]
     [(eqv? 'define (1st datum))
       ;;(valid-define? datum)
       (define-exp (2nd datum) (parse-exp (3rd datum)))]
     [(eqv? 'set! (1st datum))
       ;;(valid-set!? datum)
      (set!-exp (2nd datum) (parse-exp (3rd datum)))]
     [(memv (1st datum) '(let let* letrec))
       ;;(valid-let? datum)
      (let ([datum (if (symbol? (2nd datum)) ; check if named let
                       (cdr datum) datum)])
        (let-exp
         (1st datum)
         (map 1st (2nd datum)) ; don't parse the variable names, following 'lambda style
         (map parse-exp (map 2nd (2nd datum))) ; values of variable names
         (map parse-exp (cddr datum)))) ; bodies of let
      ]
     [(eqv? 'begin (1st datum))
      ;;(valid-begin? datum)
      (begin-exp (map parse-exp (cdr datum)))]
     [(eqv? 'and (1st datum))
      ;;(valid-and? datum)
      (and-exp (map parse-exp (cdr datum)))]
     [(eqv? 'or (1st datum))
      ;;(valid-or? datum)
      (or-exp (map parse-exp (cdr datum)))]
     [(eqv? 'case (1st datum))
      ;;(valid-case? datum)
      (case-exp
       (parse-exp (2nd datum)) ; key
       (map (lambda (v) (let ([v (1st v)]) ; convert all patterns to list
                     (if (list? v)
                         v
                         (list v)))) (cddr datum)) ; patterns
       (map (lambda (v) (map parse-exp v)) (map cdr (cddr datum))))]
     [(eqv? 'cond (1st datum))
      ;;(valid-cond? datum)
      (cond-exp (map parse-exp (map 1st (cdr datum)))
                (map (lambda (v) (map parse-exp v)) (map cdr (cdr datum))))]
      [(eqv? 'while (1st datum))
        (while-exp (parse-exp (2nd datum))
                   (map parse-exp (cddr datum)))]
     [else ; application
      (app-exp (parse-exp (1st datum))  ; rator
               (map parse-exp (cdr datum))) ; rand
      ])]
   [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))

;; Error checking functions
(define (valid-let? datum)
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




