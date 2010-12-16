#lang racket
(require "../parser.rkt"
         rackunit
         "../sanitize-ids.rkt")

(define p parse-tea-defexp)
(define (sp sexp) (sanitize-ids (p sexp)))

(define sanitize-ids-ts
  (test-suite
   "tests for sanitize-ids"
   (test-case
    "define statements"
    (check-equal? (sp '(define x 3))
                  (p '(define x 3)))
    (check-equal? (sp '(define foo-bar_ 3))
                  (p '(define foo_bar_0 3)))
    (check-equal? (sp '(define (foo-bar_ x)
                         (if (empty? x)
                             'foo-bar_
                             (foo-bar_ (rest x)))))
                  (p '(define (foo_bar_0 x)
                        (if (emptyp0 x)
                            'foo-bar_
                            (foo_bar_0 (rest x)))))))
   (test-case
    "self-evals"
    (check-equal? (sp ''x)
                  (p ''x))
    (check-equal? (sp 3)
                  (p 3))
    (check-equal? (sp "foo")
                  (p "foo")))
   (test-case
    "list literals"
    (check-equal? (sp ''(1 2 3))
                  (p ''(1 2 3))))
   (test-case
    "lambda"
    (check-equal? (sp '(lambda (x) x))
                  (p '(lambda (x) x)))
    (check-equal? (sp '(lambda (foo-bar) foo-bar))
                  (p '(lambda (foo_bar0) foo_bar0)))
    (check-equal? (sp '(lambda (foo-bar foo_bar)
                         (+ foo-bar foo-bar)))
                  (p '(lambda (foo_bar0 foo_bar)
                        (a0 foo_bar0 foo_bar0)))))
   (test-case
    "let"
    (check-equal? (sp '(let ([foo-bar 3])
                         foo-bar))
                  (p '(let ([foo_bar0 3])
                        foo_bar0)))
    (check-equal? (sp '(let ([foo_bar 4] [foo~bar 2])
                         (+ (* 10 foo_bar) foo~bar)))
                  (p '(let ([foo_bar 4] [foo_bar0 2])
                        (a0 (m0 10 foo_bar) foo_bar0)))))
   (test-case
    "nested lets and lambdas"
    ;; this occasionally fails because order is lost in the set of symbols,
    ;; but this behavior does not cause identifier collisions
    ;; verify that the identifier bindings and references are preserved
    (check-equal? (sp '(let ([foo-bar 3])
                         (let ([foo_bar 5])
                           ((lambda (x foo~bar)
                              (foo.bar foo_bar foo-bar foo~bar x))))))
                  (p '(let ([foo_bar0 3])
                        (let ([foo_bar 5])
                          ((lambda (x foo_bar2)
                             (foo_bar1 foo_bar foo_bar0 foo_bar2 x))))))))
   (test-case
    "stary-eyed fantasy -- SUCCESS!"
    (check-equal? (sp '(let ([foo-bar 3])
                         (let ([foo_bar 5])
                           (let ([foo~bar 7])
                             (procedure foo-bar foo_bar foo~bar)))))
                  (p '(let ([foo_bar0 3])
                        (let ([foo_bar 5])
                          (let ([foo_bar1 7])
                            (procedure foo_bar0 foo_bar foo_bar1)))))))))

(require rackunit/text-ui)

(run-tests sanitize-ids-ts)
