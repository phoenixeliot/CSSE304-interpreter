;; evaluator for simple expressions.
(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "interpreter.ss")))

(load-all)

(define l load-all) ; even easier!

;; Code to run repl
(define run (lambda () (begin
                    (load-all)
                    (rep))))

;; Code to run tests
(define test (lambda () (begin
                     (load-all)
                     (load "A15-test-code.ss")
                     (r))))