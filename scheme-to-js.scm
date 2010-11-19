#lang racket
(require "scheme-data.scm")
(require "js-data.scm")
(provide tea-defexp->js)

(define (tea-defexp->js scheme)
  (match scheme
    [(tea-define name value)
     (tea-define->js name value)]
    [(tea-proc-define name ids bodies)
     (tea-proc-define->js name ids bodies)]
    [(tea-symbol value)
     (tea-symbol->js value)]
    [(tea-number value)
     (tea-number->js value)]
    [(tea-string value)
     (tea-string->js value)]
    [(tea-lambda args bodies)
     (tea-lambda->js args bodies)]
    [(tea-apply head tail)
     (tea-apply->js head tail)]
    [(tea-identifier value)
     (tea-id->js value)]
    [(tea-list value)
     (tea-list->js value)]))

;; tea-defexp->js/return-last : [ListOf Tea-Exp] -> [ListOf JS-Exp]
(define (tea-defexp->js/return-last exps)
  (cond [(empty? exps) '()]
        [(empty? (rest exps))
         (cons (jreturn (tea-defexp->js (first exps))) '())]
        [else
         (cons (tea-defexp->js (first exps))
               (tea-defexp->js/return-last (rest exps)))]))

(define (tea-define->js name value)
  (jvdef (tea-id->js* name)
         (tea-defexp->js value)))

(define (tea-proc-define->js name ids bodies)
  (jfdef (tea-id->js* name)
         (map tea-id->js* ids)
         (tea-defexp->js/return-last bodies)))

(define (tea-symbol->js value)
  (jnew (jid '__tea_quote)
        (list (jstring (symbol->string value)))))

(define (tea-number->js value)
  (jnumber value))

(define (tea-string->js value)
  (jstring value))

(define (tea-lambda->js args bodies)
  (jlambda (map tea-id->js* args)
           (tea-defexp->js/return-last bodies)))

(define (tea-apply->js head tail)
  (japply (tea-defexp->js head)
          (map tea-defexp->js tail)))

(define (tea-id->js value)
  (jid value))

(define (tea-id->js* tea-id)
  (jid (tea-identifier-value tea-id)))

(define (tea-list->js value)
  (jarray (map tea-defexp->js value)))