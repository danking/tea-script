#lang racket
(require "tea-data.rkt"
         "make-uid.rkt")

(provide sanitize-exp)

(define id-map '())

(define (add-id-mapping old new id-map)
  (cons (cons old new) id-map))

(define (sanitize-tea-ids tids)
  (map (lambda (tid) (sanitize-tea-id tid)) tids))

;; sanitize-tea-id : Tea-Id -> Tea-Id
(define (sanitize-tea-id tid)
  (tea-id (sanitize-id (tea-id-value tid))))

;; sanitize-id : Symbol -> Symbol
(define (sanitize-id old-id)
  (let ([new-id (make-id-unique old-id id-map)])
    (set! id-map (add-id-mapping old-id new-id id-map))
    new-id))

(define (sanitize-exp exp)
  (set! id-map '())
  (sanitize-exp* exp))

(define (sanitize-exps* exps)
  (map (lambda (exp) (sanitize-exp* exp)) exps))

(define (sanitize-exp* exp)
  (match exp
    [(tea-define name value) (tea-define (sanitize-tea-id name)
                                         (sanitize-exp* value))]
    [(tea-pdefine name ids body) (tea-pdefine (sanitize-tea-id name)
                                              (sanitize-tea-ids ids)
                                              (sanitize-exps* body))]
    [(or (tea-symbol value)
         (tea-number value)
         (tea-string value)) exp]
    [(tea-lambda args body) (tea-lambda (sanitize-tea-ids args)
                                        (sanitize-exps* body))]
    [(tea-if c t f) (tea-if (sanitize-exp* c)
                            (sanitize-exp* t)
                            (sanitize-exp* f))]
    [(tea-let vars vals body) (tea-let (sanitize-tea-ids vars)
                                       (sanitize-exps* vals)
                                       (sanitize-exps* body))]
    [(tea-apply head tail) (tea-apply (sanitize-exp* head)
                                      (sanitize-exps* tail))]
    [(tea-id value) (sanitize-tea-id exp)]
    [(tea-list value) (tea-list (sanitize-exps* value))]))
