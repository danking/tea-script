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

(define (sanitize-exp* exp)
  (set! id-map '())
  (sanitize-exp exp))

(define (sanitize-exps exps)
  (map (lambda (exp) (sanitize-exp exp)) exps))

(define (sanitize-exp exp)
  (match exp
    [(tea-define name value) (tea-define (sanitize-tea-id name)
                                         (sanitize-exp value))]
    [(tea-pdefine name ids body) (tea-pdefine (sanitize-tea-id name)
                                              (sanitize-tea-ids ids)
                                              (sanitize-exps body))]
    [(or (tea-symbol value)
         (tea-number value)
         (tea-string value)) exp]
    [(tea-lambda args body) (tea-lambda (sanitize-tea-ids args)
                                        (sanitize-exps body))]
    [(tea-if c t f) (tea-if (sanitize-exp c)
                            (sanitize-exp t)
                            (sanitize-exp f))]
    [(tea-let vars vals body) (tea-let (sanitize-tea-ids vars)
                                       (sanitize-exps vals)
                                       (sanitize-exps body))]
    [(tea-apply head tail) (tea-apply (sanitize-exp head)
                                      (sanitize-exps tail))]
    [(tea-id value) (sanitize-tea-id exp)]
    [(tea-list value) (tea-list (sanitize-exps value))]))

(require rackunit
         "parser.rkt")

(define p parse-tea-defexp)
(define (sp sexp) (sanitize-exp* (p sexp)))

(check-equal? (sp '(define x 3))
              (p '(define x 3)))
(check-equal? (sp '(define foo-bar_ 3))
              (p '(define foo_bar_0 3)))
(check-equal? (sp '(define (foo-bar_ x)
                     (if (empty? x)
                         'foo-bar_
                         (foo-bar_ (rest x)))))
              (p '(define (foo_bar_0 x)
                    (if (empty? x)
                        'foo-bar_
                        (foo_bar_0 (rest x))))))
(check-equal? (sp ''x)
              (p ''x))
(check-equal? (sp 3)
              (p 3))
(check-equal? (sp "foo")
              (p "foo"))
(check-equal? (sp '(lambda (x) x))
              (p '(lambda (x) x)))
(check-equal? (sp '(lambda (foo-bar) foo-bar))
              (p '(lambda (foo_bar0) foo_bar0)))
(check-equal? (sp '(lambda (foo-bar foo_bar)
                     (+ foo-bar foo-bar)))
              (p '(lambda (foo_bar0 foo_bar)
                    (a0 foo_bar0 foo_bar0))))
(check-equal? (sp '(let ([foo-bar 3])
                     foo-bar))
              (p '(let ([foo_bar0 3])
                    foo_bar0)))
(check-equal? (sp '(let ([foo_bar 4] [foo~bar 2])
                     (+ (* 10 foo_bar) foo~bar)))
              (p '(let ([foo_bar 4] [foo_bar0 2])
                    (a0 (m0 10 foo_bar) foo_bar0))))
(check-equal? (sp '(let ([foo-bar 3])
                     (let ([foo_bar 5])
                       ((lambda (x foo~bar)
                          (foo.bar foo_bar foo-bar foo~bar x))))))
              (p '(let ([foo_bar0 3])
                    (let ([foo_bar 5])
                      ((lambda (x foo_bar1)
                         (foo_bar2 foo_bar foo_bar0 foo_bar1 x)))))))

; (require rackunit/text-ui)

; (run-tests)