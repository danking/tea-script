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
  (match-tea exp sanitize-exp* sanitize-tea-id))
