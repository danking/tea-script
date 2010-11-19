#lang scheme
(require "../string-utilities.scm")
(require test-engine/racket-tests)

;; ->csv
(check-expect (->csv '(1 2 3)
                     number->string)
              "1, 2, 3")
(check-expect (->csv '(a b c)
                     symbol->string)
              "a, b, c")
(check-expect (->csv '(1 a "s")
                     (lambda (x)
                       (format "~a" x)))
              "1, a, s")

(define (->string x)
  (cond [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [(string? x) x]
        [(cons?   x) (string-append "[" (->csv x ->string) "]")]))

(check-expect (->csv '(1 a (3 4)) ->string)
              "1, a, [3, 4]")

;; string-repeat
(check-expect (string-repeat 5 " ") "     ")
(check-expect (string-repeat 0 " ") "")
(check-expect (string-repeat 2 "  ") "    ")
(check-expect (string-repeat 3 "\t") "\t\t\t")

(test)