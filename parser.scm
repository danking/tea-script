#lang racket
(require "tea-data.scm")
(provide parse-tea-defexp)

(define (parse-tea-defexp defexp)
  (match defexp
    [(list 'define _ ..1) (parse-tea-def defexp)]
    [_                    (parse-tea-exp defexp)]))

(define (parse-tea-def def)
  (match def
    [(list 'define (list name ids ...) bodies ..1)
     (make-tea-proc-define (parse-tea-id name)
                           (map parse-tea-id ids)
                           (map parse-tea-exp bodies))]
    [(list 'define id body) (make-tea-define (parse-tea-id id)
                                             (parse-tea-exp body))]
    [_ (error 'lexer "expected ~s to be a definition")]))

(define (parse-tea-exp exp)
  (match exp
    [(? string? _) (make-tea-string exp)]
    [(? number? _) (make-tea-number exp)]
    [(? symbol? _) (make-tea-identifier exp)]
    [(list 'quote (? symbol? s)) (make-tea-symbol s)]
    [(list 'quote ls) (make-tea-list (map parse-tea-data ls))]
    [(list 'lambda (list ids ...) bodies ..1)
     (make-tea-lambda (map parse-tea-id ids)
                      (map parse-tea-exp bodies))]
    [(list head tail ...)
     (make-tea-apply  (parse-tea-exp head)
                      (map parse-tea-exp tail))]
    [_ (error 'lexer "expected ~s to be an expression" exp)]))

(define (parse-tea-id id)
  (if (symbol? id) (make-tea-identifier id)
      (error 'lexer "expected ~s to be an identifier" id)))

(define (parse-tea-data data)
  (match data
    [(? string? _) (make-tea-string data)]
    [(? number? _) (make-tea-number data)]
    [(? symbol? _) (make-tea-symbol data)]
    [(list data ...)
     (make-tea-list (map parse-tea-data data))]
    [_ (error 'lexer "expected ~s to be a datum" exp)]))
