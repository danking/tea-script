#lang racket
(require "parser.rkt"
         "scheme-to-js.rkt"
         "js-to-text.rkt")
(provide tea->js)

(define (tea->js sexp)
  (jstatement->text (tea-defexp->js (parse-tea-defexp sexp))))