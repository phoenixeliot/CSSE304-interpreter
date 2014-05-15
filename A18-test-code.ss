;; Test code for CSSE 304 Assignment 17

(define (test-set!-local)
  (let ([correct '(
                   73
                   93
                   19
                   8
                   43
                   )]
        [answers 
         (list 
          (eval-one-exp '(let ([f #f] [x 3]) (set! f (lambda (n) (+ 3 (* n 10)))) (set! x 7) (f x)))
          (eval-one-exp '((lambda (x) (set! x (+ x 1)) (+ x 2)) 90))
          (eval-one-exp '(let ([x 5] [y 3]) (let ([z (begin (set! x (+ x y)) x)]) (+ z (+ x y)))))
          (eval-one-exp '(let ([a 5]) (if (not (= a 6)) (begin (set! a (+ 1 a)) (set! a (+ 1 a))) 3) (+ 1 a)))
          (eval-one-exp '(let ([f #f]) (let ([dummy (begin (set! f (lambda (n) (+ 3 (* n 10)))) 3)]) (f 4))))


          )])
    (display-results correct answers equal?)))

(define (test-simple-defines)
  (let ([correct '(
                   8
                   13
                   (12 14)
                   32
                   )]
        [answers 
         (list 
          (eval-one-exp ' (begin (define a 5) (+ a 3)))
          (eval-one-exp ' (begin (define c 5) (define d (+ c 2)) (+ d (add1 c))))
          (eval-one-exp ' (begin (define e 5) (let ([f (+ e 2)]) (set! e (+ e f)) (set! f (* 2 f)) (list e f))))
          (eval-one-exp ' (begin (define ff (letrec ([ff (lambda (x) (if (= x 1) 2 (+ (* 2 x) (ff (- x 2)))))]) ff)) (ff 7)))

          )])
    (display-results correct answers equal?)))

(define (test-letrec-and-define)
  (let ([correct '(
                   55
                   55
                   773

                   )]
        [answers 
         (list 
          (begin (reset-global-env) (eval-one-exp '(letrec ([f (lambda (n) (if (= n 0) 0 (+ n (f (sub1 n)))))]) (f 10))))
          (begin (reset-global-env) (eval-one-exp '(define zer0? (lambda (x) (= x 0)))) (eval-one-exp '(letrec ([f (lambda (n) (if (zer0? n) 0 (+ n (f (sub1 n)))))]) (f 10))))
          (eval-one-exp '(letrec ([f (lambda (n) (if (zero? n) 0 (+ 4 (g (sub1 n)))))] [g (lambda (n) (if (zero? n) 0 (+ 3 (f (sub1 n)))))]) (g (f (g (f 5))))))

          )])
    (display-results correct answers equal?)))





(define (test-named-let-and-define)
  (let ([correct '(
                   120
                   120

                   )]
        [answers 
         (list 
          (eval-one-exp '(begin (define fact (lambda (n) (let loop ((n n) (m 1)) (if (= n 0) m (loop (- n 1) (* m n)))))) (fact 5)))
          (eval-one-exp '(let fact ((n 5) (m 1)) (if (= n 0) m (fact (- n 1) (* m n)))))

          )])
    (display-results correct answers equal?)))



(define (test-set!-global)
  (let ([correct '(
                   7
                   4
                   120
                   9

                   )]
        [answers 
         (list 
          (begin (reset-global-env) (eval-one-exp '(define a 3)) (eval-one-exp '(set! a 7)) (eval-one-exp 'a))
          (begin (reset-global-env) (eval-one-exp '(define a 3)) (eval-one-exp '(define f '())) (eval-one-exp '(set! f (lambda (x) (+ x 1)))) (eval-one-exp '(f a)))
          (begin (reset-global-env) (eval-one-exp '(define a 5)) (eval-one-exp '(define f '())) (eval-one-exp '(set! f (lambda (x) (if (= x 0) 1 (* x (f (- x 1))))))) (eval-one-exp '(f a)))
          (begin (reset-global-env) (eval-one-exp '(define a 5)) (eval-one-exp '(let ([b 7]) (set! a 9))) (eval-one-exp 'a))

          )])
    (display-results correct answers equal?)))


(define (test-ref-parameters)
  (let ([correct '(
                   (4 3)
                   (4 4)

                   )]
        [answers 
         (list 
          (eval-one-exp ' (let ([a 3] [b 4] [swap! (lambda ((ref x) (ref y)) (let ([temp x]) (set! x y) (set! y temp)))]) (swap! a b) (list a b)))
          (eval-one-exp ' (let ([a 3] [b 4] [swap (lambda ((ref x) y) (let ([temp x]) (set! x y) (set! y temp)))]) (swap a b) (list a b)))

          )])
    (display-results correct answers equal?)))


(define (test-order-matters)
  (let ([correct '(
                   (30 (29 27 24 20 15 9 2 3) 0)
                   55
                   )]
        [answers 
         (list 
          (eval-one-exp ' (let ([r 2] [ls '(3)] [count 7]) (let loop () (if (> count 0) (begin (set! ls (cons r ls)) (set! r (+ r count)) (set! count (- count 1)) (loop)) )) (list r ls count)))
          (eval-one-exp ' (begin (define latest 1) (define total 1) (or (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50)) (begin (set! latest (+ latest 1)) (set! total (+ total latest)) (> total 50))) total))

          )])
    (display-results correct answers equal?)))

                                        ;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
    (display ": ")
    (pretty-print 
     (if (andmap test-procedure? correct results)
         'All-correct
         `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))

(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
                                        ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)






                                        ;You can run the tests individually, or run them all
                                        ;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'set!-local) 
  (test-set!-local)
  (display 'simple-defines) 
  (test-simple-defines)
  (display 'letrec-and-define) 
  (test-letrec-and-define)
  (display 'named-let-and-define) 
  (test-named-let-and-define)
  (display 'set!-global) 
  (test-set!-global)
  (display 'ref-parameters) 
  (test-ref-parameters)
  (display 'order-matters) 
  (test-order-matters)
  )

(define r run-all)
