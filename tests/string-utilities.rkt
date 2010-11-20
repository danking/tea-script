#lang racket
(require "../string-utilities.rkt")
(require test-engine/racket-tests)

;; ->csv
(check-expect (->csv number->string
                     '(1 2 3))
              "1, 2, 3")
(check-expect (->csv symbol->string
                     '(a b c))
              "a, b, c")
(check-expect (->csv (lambda (x)
                       (format "~a" x))
                     '(1 a "s"))
              "1, a, s")

(define (->string x)
  (cond [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [(cons?   x) (string-append "[" (->csv ->string x) "]")]))

(check-expect (->csv ->string '(1 a (3 4)))
              "1, a, [3, 4]")

;; string-repeat
(check-expect (string-repeat 5 " ") "     ")
(check-expect (string-repeat 0 " ") "")
(check-expect (string-repeat 2 "  ") "    ")
(check-expect (string-repeat 3 "\t") "\t\t\t")

(test)