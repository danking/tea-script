#lang racket
(require "eval.rkt")

;;; THIS IS EXPERIMENTAL.
;;; IT DOES NOT ACCUMULATE AN ENVIRONMENT.
;;; DON'T USE IT EXCEPT FOR FUN AND STUFF.
(define (tea-eval/repl stx)
  (let* ([sexp (cdr (syntax->datum stx))])
    (tea-eval sexp)))

(current-eval tea-eval/repl)
(current-print (lambda (value)
                 (unless (or (void? value)
                             (and (string? value)
                                  (string=? value "js> ")))
                   (displayln value))))       ; print strings without the quotes
(read-eval-print-loop)
