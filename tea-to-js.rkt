#lang racket
(require "tea-data.rkt"
         "js-data.rkt"
         "make-uid.rkt") ; this one is for create-jdot-or-jbracket
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
    [(tea-cond conditions results else)
     (tea-cond->js conditions results else)]
    [(tea-let vars vals body)
     (tea-let->js vars vals body)]
    [(tea-let* vars vals body)
     (tea-let*->js vars vals body)]
    [(tea-letrec vars vals body)
     (tea-letrec->js vars vals body)]
    [(tea-apply head tail)
     (tea-apply->js head tail)]
    [(tea-id value)
     (tea-id->js value)]
    [(tea-list value)
     (tea-list->js value)]
    [(tea-raise value)
     (tea-raise->js value)]
    [(tea-void)
     (jnull)]
    [(tea-send object method args)
     (tea-send->js object method args)]
    [(tea-get-field object field)
     (tea-get-field->js object field)]))

(define (tea-defexps->js tea-exps)
  (map tea-defexp->js tea-exps))

;; tea-defexps->js/return-last : [ListOf Tea-Exp] -> [ListOf JS-Exp]
(define (tea-defexps->js/return-last exps)
  (cond [(empty? exps) '()]
        [(empty? (rest exps))
         (cons (jreturn (tea-defexp->js (first exps))) '())]
        [else
         (cons (tea-defexp->js (first exps))
               (tea-defexps->js/return-last (rest exps)))]))

(define (tea-define->js name value)
  (jvdef (tea-id->js* name)
         (tea-defexp->js value)))

(define (tea-pdefine->js name ids bodies)
  (jfdef (tea-id->js* name)
         (tea-ids->js* ids)
         (tea-defexps->js/return-last bodies)))

(define (tea-symbol->js value)
  (jnew (jid '__tea_quote)
        (list (jstring (symbol->string value)))))

(define (tea-number->js value)
  (jnumber value))

(define (tea-string->js value)
  (jstring value))

(define (tea-lambda->js args body)
  (jlambda (tea-ids->js* args)
           (tea-defexps->js/return-last body)))

(define (tea-if->js c t f)
  (jcond (tea-defexp->js c)
         (tea-defexp->js t)
         (tea-defexp->js f)))

(define (tea-cond->js conditions results else)
  (foldr (lambda (condition result jcond-expr)
           (jcond (tea-defexp->js condition)
                  (tea-defexp->js result)
                  jcond-expr))
         (tea-defexp->js else)
         conditions
         results))

(define (tea-let->js vars vals body)
  (japply (jlambda (tea-ids->js* vars)
                   (tea-defexps->js/return-last body))
          (tea-defexps->js vals)))

(define (tea-let*->js vars vals body)
  (tea-defexp->js (foldr (lambda (var val tea-expr)
                           (tea-let (list var) (list val) (if (cons? tea-expr)
                                                              tea-expr
                                                              (list tea-expr))))
                         body
                         vars
                         vals)))

(define (tea-letrec->js vars vals body)
  (japply (jlambda (list)
                   (append (map (lambda (var val)
                                  (jvdef (tea-defexp->js var)
                                         (tea-defexp->js val)))
                                vars vals)
                           (tea-defexps->js/return-last body)))
          (list)))

(define (tea-apply->js head tail)
  (japply (tea-defexp->js head)
          (tea-defexps->js tail)))

(define (tea-raise->js value)
  (jthrow (tea-defexp->js value)))

(define (tea-send->js object method args)
  (japply (create-jdot-or-jbracket (tea-defexp->js object)
                                   (tea-defexp->js method))
          (tea-defexps->js args)))

(define (tea-get-field->js object field)
  (create-jdot-or-jbracket (tea-defexp->js object)
                           (tea-defexp->js field)))

(define (create-jdot-or-jbracket object property)
  (if (jid? property)
      (let [(identifier-symbol (jid-name property))]
        (if (not (bad-id? identifier-symbol))
            (jdot object property)
            (jbracket object (jstring (symbol->string identifier-symbol)))))
      (jbracket object property)))

(define (tea-id->js value)
  (jid value))

(define (tea-ids->js* tea-ids)
  (map (lambda (tea-id) (jid (tea-id-value tea-id))) tea-ids))

(define (tea-id->js* tea-id)
  (jid (tea-id-value tea-id)))

(define (tea-list->js value)
  (jarray (tea-defexps->js value)))