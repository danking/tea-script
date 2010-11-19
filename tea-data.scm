#lang scheme
(provide (all-defined-out))
(define-struct tea-define (name value) #:transparent)
(define-struct tea-proc-define (name ids bodies) #:transparent)
(define-struct tea-symbol (value) #:transparent)
(define-struct tea-number (value) #:transparent)
(define-struct tea-string (value) #:transparent)
(define-struct tea-lambda (args bodies) #:transparent)
(define-struct tea-apply  (head tail) #:transparent)
(define-struct tea-identifier (value) #:transparent)
(define-struct tea-list   (value) #:transparent)

;; a Tea-Define is a (make-tea-define Tea-Identifier Tea-Expression)

;; a Tea-Proc-Define is a (make-tea-proc-define Tea-Identifier
;;                                              [ListOf Tea-Identifier]
;;                                              [ListOf Tea-Expression])

;; a Tea-Symbol is a (make-tea-symbol Symbol)

;; a Tea-Number is a (make-tea-number Number)

;; a Tea-String is a (make-tea-string String)

;; a Tea-Lambda is a (make-tea-lambda [ListOf Tea-Identifier]
;;                                    [ListOf Tea-Expression])

;; a Tea-Apply  is a (make-tea-apply Tea-Expression
;;                                   Tea-Expression)

;; a Tea-Identifier is a (make-tea-identifier Symbol)

;; a Tea-List   is a (make-tea-list [ListOf Tea-Expression])