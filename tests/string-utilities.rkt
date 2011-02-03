#lang racket
(require rackunit
         "../string-utilities.rkt")
(provide string-utilities-ts)

(define (->string x)
  (cond [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [(cons?   x) (string-append "[" (->csv ->string x) "]")]))

;; ->csv
(define string-utilities-ts
  (test-suite
   "tests for the string utilities"
   (test-case
    "->csv"
    (check-equal? (->csv number->string
                         '(1 2 3))
                  "1, 2, 3")
    (check-equal? (->csv symbol->string
                         '(a b c))
                  "a, b, c")
    (check-equal? (->csv (lambda (x)
                           (format "~a" x))
                         '(1 a "s"))
                  "1, a, s")
    (check-equal? (->csv ->string '(1 a (3 4)))
                  "1, a, [3, 4]"))
   (test-case
    "string-repeat"
    (check-equal? (string-repeat 5 " ") "     ")
    (check-equal? (string-repeat 0 " ") "")
    (check-equal? (string-repeat 2 "  ") "    ")
    (check-equal? (string-repeat 3 "\t") "\t\t\t"))))
