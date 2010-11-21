#lang racket
(require "parser.rkt"
         "tea-to-js.rkt"
         "js-expansions.rkt"
         "js-to-text.rkt")
(provide tea->js)

(define (tea->js sexp)
  (jstatement->text (expand-js (tea-defexp->js (parse-tea-defexp sexp)))))