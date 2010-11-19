#lang scheme
(require "parser.scm"
         "scheme-to-js.scm"
         "js-to-text.scm")
(provide tea->js)

(define (tea->js sexp)
  (jstatement->text (tea-defexp->js (parse-tea-defexp sexp))))