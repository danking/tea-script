#lang racket
(require "tea-data.rkt"
         "make-uid.rkt")
(provide sanitize-ids)

(define (sanitize-ids exp)
  (let ([id-map (sanitize-symbols (gather-ids exp))])
    (sanitize-ids/id-map exp id-map)))

(define (sanitize-ids/id-map exp id-map)
  (let ([sanitize-expression (lambda (exp) (sanitize-ids/id-map exp id-map))]
        [curried-sanitize-id (lambda (id) (sanitize-id id id-map))])
    (match-tea exp sanitize-expression curried-sanitize-id
               [(tea-send object method args)
                (tea-send (sanitize-expression object)
                          method
                          (map sanitize-expression args))]
               [(tea-get-field object field)
                (tea-get-field (sanitize-expression object) field)])))

(define (sanitize-id id id-map)
  (tea-id (hash-ref id-map
                    (tea-id-value id)
                    (lambda ()
                      (error 'sanitize-ids
                             (string-append
                              "somehow we didn't record an id in "
                              "the gather-ids phase; end users "
                              "should never see this -- ~a ~a")
                             id
                             id-map)))))

;; gather-ids : Tea-Expression -> [ListOf Symbol]
(define (gather-ids exp)
  (set->list (gather-ids/set exp)))

(define (gather-ids/set exp)
  (tea-accumulator exp
                   (lambda (list-of-sets)
                     (foldl (lambda (subset superset)
                              (set-union subset superset))
                            (set)
                            list-of-sets))
                   gather-ids/set
                   (lambda (id) (set (tea-id-value id)))))

;; set->list : [SetOf X] -> [ListOf X]
(define (set->list set)
  (set-map set (lambda (x) x)))

;; sanitize-symbols : [ListOf Symbol] -> [Hasheq Symbol -> Symbol]
(define (sanitize-symbols ids)
  (let loop ([ids-to-make-unique ids]
             [id-map (hasheq)])
    (if (empty? ids-to-make-unique)
        id-map
        (let* ([old-id (first ids-to-make-unique)]
               [new-id (make-symbol-unique old-id (hash-values id-map))])
          (loop (rest ids-to-make-unique)
                (hash-set id-map old-id new-id))))))
