#lang racket
(require "tea-data.rkt"
         "make-uid.rkt")
(provide sanitize-exp)

(define id-map '())


(define (sanitize-exp exp)
  (set! id-map '())
  (sanitize-exp* exp))

(define (sanitize-exp* exp)
  (match-tea exp sanitize-exp* sanitize-tea-id))

;; add-id-mapping : Symbol Symbol [ListOf Symbol]
(define (add-id-mapping old new)
  (cons (cons old new) id-map))

;; sanitize-tea-id : Tea-Id -> Tea-Id
(define (sanitize-tea-id tid)
  (tea-id (sanitize-symbol (tea-id-value tid))))

;; sanitize-id : Symbol -> Symbol
(define (sanitize-symbol old-id)
  (let ([new-id (make-id-unique old-id id-map)])
    (set! id-map (add-id-mapping old-id new-id))
    new-id))
