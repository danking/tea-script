#lang scheme
(require "../syntax-data.scm")
(require "../js-data.scm")
(provide (all-defined-out))

;; testing convienence syntax procedures
(define tdefine make-tea-define)
(define tpdefine make-tea-proc-define)
(define tsymbol make-tea-symbol)
(define tnumber make-tea-number)
(define tstring make-tea-string)
(define tlambda make-tea-lambda)
(define tapply  make-tea-apply)
(define tid     make-tea-identifier)
(define tls     make-tea-list)

;; testing convienence general procedures
(define sa      string-append)
