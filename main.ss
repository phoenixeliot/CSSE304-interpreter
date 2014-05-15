;; evaluator for simple expressions.
(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "interpreter.ss")
    (load "syntax-expand.ss")))

(load-all)

(define l load-all) ; even easier!

;; Function to run repl
(define run (lambda () (begin
                    (load-all)
                    (rep))))

;; Function to run debug repl
(define run-debug (lambda () (begin
                          (load-all)
                          (rep-debug))))

;; Function to run tests
(define test (lambda () (begin
                     (display "======= A15 TESTS =======\n") 
                     (load-all)
                     (load "A15-test-code.ss")
                     (r)
                     (display "======= A16 TESTS =======\n")
                     (load "A16-test-code.ss")
                     (r)
                     (display "======= A17 TESTS =======\n")
                     (load "A17-test-code.ss")
                     (r)
                     (display "======= A18 TESTS =======\n")
                     (load "A18-test-code.ss")
                     (r)
                     )))
