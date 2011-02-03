#lang racket
(require rackunit
         "../eval.rkt")
(provide eval-ts)

;; symbols
(define eval-ts
 (test-suite
  "runtime tests"
  (test-case
   "non-js friendly ids"
   (check-equal? (tea-eval '(symbol=? 'foo-bar_ 'foo_bar-))
                  "false"))            ; depends on gensym
  (test-case
   "addition"
   (check-equal? (tea-eval '(+ 1 2 3))
                 "6"))
  (test-case
   "lists"
   (check-equal? (tea-eval ''(1 2 3))
                 "1,2,3")
   (check-equal? (tea-eval ''(1 2 ("a" () "b") "foo"))
                 (string-append "1,2,a,,b,foo")))
  (test-case
   "map"
   (check-equal? (tea-eval '(map (lambda (x) (* x x)) '(1 2 3 4 5 6)))
                 "1,4,9,16,25,36"))
  (test-case
   "foldr"
   (check-equal? (tea-eval '(foldr cons '() '(1 2 3 4 5)))
                 "1,2,3,4,5"))
  (test-case
   "foldl"
   (check-equal? (tea-eval '(foldl cons '() '(1 2 3 4 5)))
                 "5,4,3,2,1"))
  (test-case
   "let"
   (check-equal? (tea-eval '(let ([x 3] [y 5])
                              (+ x y 2)))
                 "10"))))
