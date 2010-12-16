#lang racket
(require rackunit
         "../eval.rkt")
(provide eval-ts)

(define-syntax check-runtime
  (syntax-rules ()
   [(_ sexp expected) (let [(output (tea-eval sexp))]
                        (check-equal? (first output)
                                      expected
                                      (second output)))]))

;; symbols
(define eval-ts
 (test-suite
  "runtime tests"
  (test-case
   "non-js friendly ids"
   (check-runtime '(symbol=? foo-bar_ foo_bar-)
                  "false"))            ; depends on gensym
  (test-case
   "let"
   (check-runtime '(let ([x 3] [y 5])
                        (+ x y 2))
                  "10"))))

(require rackunit/text-ui)

(run-tests eval-ts)