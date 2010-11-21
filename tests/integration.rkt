#lang racket
(require rackunit
         "../parser.rkt"
         "../tea-to-js.rkt"
         "../js-to-text.rkt")

(define sa string-append)
(define (translate sexp)
  (jstatement->text (tea-defexp->js (parse-tea-defexp sexp))))


(define integration-ts
  (test-suite
   "integration tests"
   (test-case
    "let"
    (check-equal? (translate '(let ([x 3] [y "foo"])
                                (foo x y)))
                  (sa "(function (x, y) {\n"
                      "  return foo(x, y);\n"
                      "})(3, \"foo\");\n")))
   (test-case
    "if"
    (check-equal? (translate '(if (predicate foo bar)
                                  (branch1 foo bar)
                                  (branch2 foo bar)))
                  (sa "(predicate(foo, bar) ? branch1(foo, bar) : "
                      "branch2(foo, bar));\n")))
   (test-case
    "function definition"
    (check-equal? (translate '(define (f x)
                                (if (predicate x) 3 (f (sub1 x)))))
                  (sa "function f (x) {\n"
                      "  return (predicate(x) ? 3 : f(sub1(x)));\n"
                      "};\n")))))

(require rackunit/text-ui)

(run-tests integration-ts)
