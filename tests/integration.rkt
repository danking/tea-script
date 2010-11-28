#lang racket
(require rackunit
         "../main.rkt")

(define sa string-append)

(define integration-ts
  (test-suite
   "integration tests"
   (test-suite
    "statement integration tests"
    (test-case
     "let"
     (check-equal? (tea->js '(let ([x 3] [y "foo"])
                               (foo x y)))
                   (sa "(function (x0, y0) {\n"
                       "  return foo0(x0, y0);\n"
                       "})(3, \"foo\");\n")))
    (test-case
     "if"
     (check-equal? (tea->js '(if (predicate foo bar)
                                 (branch1 foo bar)
                                 (branch2 foo bar)))
                   (sa "(predicate0(foo0, bar0) ? branch10(foo0, bar0) : "
                       "branch20(foo0, bar0));\n")))
    (test-case
     "raise"
     (check-equal? (tea->js '(raise "OH SHIT!"))
                   "throw \"OH SHIT!\";\n"))
    (test-case
     "function definition"
     (check-equal? (tea->js '(define (f x)
                               (if (predicate x) 3 (f (sub1 x)))))
                   (sa "function f0 (x0) {\n"
                       "  return (predicate0(x0) ? 3 : f0(sub10(x0)));\n"
                       "};\n"))))
   (test-suite
    "program integration tests"
    (test-case
     "define and use"
     (check-equal? (tea-program->js-program '((define foo 3)
                                              (+ foo 6)))
                   (sa "var foo0 = 3;\n"
                       "a0(foo0, 6);\n")))
    (test-case
     "factorial and use"
     (check-equal? (tea-program->js-program '((define (factorial x)
                                                (if (< x 2)
                                                    1
                                                    (* x
                                                       (factorial (sub1 x)))))
                                              (factorial 5)))
                   (sa "function factorial0 (x0) {\n"
                       "  return (l0(x0, 2) ? 1 : "
                       "m0(x0, factorial0(sub10(x0))));\n"
                       "};\n"
                       "factorial0(5);\n"))))))

(require rackunit/text-ui)

(run-tests integration-ts)
