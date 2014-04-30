
;; Parsed expression datatypes

(define (void? x) (equal? x (void)))

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
    (datum
      (lambda (x)
        (ormap 
          (lambda (pred) (pred x))
          (list number? vector? boolean? symbol? string? pair? null? void?))))]
  [if-exp
    (condition expression?)
    (true-body expression?)
    (false-body expression?)]
  [let-exp
    (vars (list-of symbol?))
    (values (list-of expression?))
    (bodies (list-of expression?))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))])

	
;; datatype for procedures.  At first there is only one
;; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	 
	 
	 
	
;; environment type definitions
(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
