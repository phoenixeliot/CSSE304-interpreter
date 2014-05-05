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
                     (load "main.ss")
                     (rep))))