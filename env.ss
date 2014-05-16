;; Environment definitions
(define (empty-env)
  (empty-env-record))

(define (extend-env syms vals env)
  (extended-env-record syms (map box vals) env))

(define (list-find-position sym los)
  (list-index (lambda (xsym) (eqv? sym xsym)) los))

(define (list-index pred ls)
  (cond
   [(null? ls) #f]
   [(pred (car ls)) 0]
   [else (let ((list-index-r (list-index pred (cdr ls))))
           (if (number? list-index-r)
               (+ 1 list-index-r)
               #f))]))

;; Lookup env-ref in environment
;; succeed and fail are procedures applied if the var is or isn't found, respectively.
(define (apply-env-ref env sym succeed fail) 
  (cases environment env
         [empty-env-record ()
                           (fail)]
         [extended-env-record (syms vals env)
                              (let ([pos (list-find-position sym syms)])
                                (if (number? pos)
                                    (succeed (list-ref vals pos))
                                    (apply-env-ref env sym succeed fail)))]))

;; Dereference a ref in an envrironment
(define (deref ref)
  (unbox ref))

;; Set a reference in an environment
(define (set-ref! ref val)
  (set-box! ref val))

;; Get the value of a refernece in an environment
(define (apply-env env sym succeed fail)
  (apply-env-ref env sym (lambda (ref)(succeed (deref ref))) fail))

(define (apply-env-with-global sym env)
  (apply-env env sym 
             (lambda (v) v) ; procedure to call if id is in the environment 
             (lambda ()  ; procedure to call if id not in env
               (apply-env global-env sym
                          (lambda (v) v)
                          (lambda () (eopl:error 'apply-env-with-global
                                            "variable not found in environment: ~s"
                                            sym))))))

(define (apply-env-ref-with-global sym env)
  (apply-env-ref env sym
                 (lambda (v) v)
                 (lambda () (apply-env-ref global-env sym
                                      (lambda (v) v)
                                      (lambda () (eopl:error 'apply-env-ref-with-global
                                                        "variable not found in environment: ~s"
                                                        sym))))))
