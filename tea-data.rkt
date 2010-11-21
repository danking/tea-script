#lang racket
(provide (except-out (all-defined-out) tstruct))

(define-syntax tstruct
  (syntax-rules ()
    [(_ args ...) (struct args ... #:transparent)]))

(tstruct tea-define (name value))
(tstruct tea-pdefine (name ids bodies))
(tstruct tea-symbol (value))
(tstruct tea-number (value))
(tstruct tea-string (value))
(tstruct tea-lambda (args bodies))
(tstruct tea-if     (c t f))
(tstruct tea-let    (vars vals body))
(tstruct tea-apply  (head tail))
(tstruct tea-id     (value))
(tstruct tea-list   (value))

;; a Tea-Define is a (tea-define Tea-Identifier Tea-Expression)

;; a Tea-Proc-Define is a (tea-proc-define Tea-Identifier
;;                                         [ListOf Tea-Identifier]
;;                                         [ListOf Tea-Expression])

;; a Tea-Symbol is a (tea-symbol Symbol)

;; a Tea-Number is a (tea-number Number)

;; a Tea-String is a (tea-string String)

;; a Tea-Lambda is a (tea-lambda [ListOf Tea-Identifier]
;;                               [ListOf Tea-Expression])

;; a Tea-if     is a (tea-if Tea-Expression Tea-Expression Tea-Expression)

;; a Tea-Let    is a (tea-let [ListOf Tea-Identifier]
;;                            [ListOf Tea-Expression]
;;                            [ListOf Tea-Expression])

;; a Tea-Apply  is a (tea-apply Tea-Expression
;;                              Tea-Expression)

;; a Tea-Identifier is a (tea-identifier Symbol)

;; a Tea-List   is a (tea-list [ListOf Tea-Expression])