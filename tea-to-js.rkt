#lang racket
(require "tea-data.rkt"
         "js-data.rkt")
(provide tea-defexp->js)

(define (tea-defexp->js tea-exp)
  (match tea-exp
    [(tea-define name value)
     (tea-define->js name value)]
    [(tea-pdefine name ids bodies)
     (tea-pdefine->js name ids bodies)]
    [(tea-symbol value)
     (tea-symbol->js value)]
    [(tea-number value)
     (tea-number->js value)]
    [(tea-string value)
     (tea-string->js value)]
    [(tea-lambda args bodies)
     (tea-lambda->js args bodies)]
    [(tea-if c t f)
     (tea-if->js c t f)]
    [(tea-let vars vals body)
     (tea-let->js vars vals body)]
    [(tea-apply head tail)
     (tea-apply->js head tail)]
    [(tea-id value)
     (tea-id->js value)]
    [(tea-list value)
     (tea-list->js value)]))

(define (tea-defexps->js tea-exps)
  (map tea-defexp->js tea-exps))

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

(define (tea-pdefine->js name ids bodies)
  (jfdef (tea-id->js* name)
         (tea-ids->js* ids)
         (tea-defexp->js/return-last bodies)))

(define (tea-symbol->js value)
  (jnew (jid '__tea_quote)
        (list (jstring (symbol->string value)))))

(define (tea-number->js value)
  (jnumber value))

(define (tea-string->js value)
  (jstring value))

(define (tea-lambda->js args body)
  (jlambda (tea-ids->js* args)
           (tea-defexp->js/return-last body)))

(define (tea-if->js c t f)
  (jcond (tea-defexp->js c)
         (tea-defexp->js t)
         (tea-defexp->js f)))

(define (tea-let->js vars vals body)
  (japply (jlambda (tea-ids->js* vars)
                   (tea-defexp->js/return-last body))
          (tea-defexps->js vals)))

(define (tea-apply->js head tail)
  (japply (tea-defexp->js head)
          (tea-defexps->js tail)))

(define (tea-id->js value)
  (jid value))

(define (tea-ids->js* tea-ids)
  (map (lambda (tea-id) (jid (tea-id-value tea-id))) tea-ids))

(define (tea-id->js* tea-id)
  (jid (tea-id-value tea-id)))

(define (tea-list->js value)
  (jarray (tea-defexps->js value)))