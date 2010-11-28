#lang racket
(require "parser.rkt"
         "sanitize-ids.rkt"
         "tea-to-js.rkt"
         "js-expansions.rkt"
         "js-to-text.rkt")
(provide tea->js tea-program->js-program)

;; tea->js : SExp
(define (tea->js sexp)
  (jstatement->text
   (expand-js (tea-defexp->js (sanitize-exp (parse-tea-defexp sexp))))))

;; tea-program->js-program : [ListOf SExp]
(define (tea-program->js-program sexps)
  (foldr (lambda (statement program)
           (string-append (tea->js statement)
                          program))
         ""
         sexps))