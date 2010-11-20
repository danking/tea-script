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
     (tea-pdefine (parse-tea-id name)
                           (map parse-tea-id ids)
                           (map parse-tea-exp bodies))]
    [(list 'define id body) (tea-define (parse-tea-id id)
                                             (parse-tea-exp body))]
    [_ (error 'lexer "expected ~s to be a definition")]))

(define (parse-tea-exp exp)
  (match exp
    [(? string? _) (tea-string exp)]
    [(? number? _) (tea-number exp)]
    [(? symbol? _) (tea-id exp)]
    [(list 'quote (? symbol? s)) (tea-symbol s)]
    [(list 'quote ls) (tea-list (map parse-tea-data ls))]
    [(list 'lambda (list ids ...) bodies ..1)
     (tea-lambda (map parse-tea-id ids)
                      (map parse-tea-exp bodies))]
    [(list head tail ...)
     (tea-apply  (parse-tea-exp head)
                      (map parse-tea-exp tail))]
    [_ (error 'lexer "expected ~s to be an expression" exp)]))

(define (parse-tea-id id)
  (if (symbol? id) (tea-id id)
      (error 'lexer "expected ~s to be an identifier" id)))

(define (parse-tea-data data)
  (match data
    [(? string? _) (tea-string data)]
    [(? number? _) (tea-number data)]
    [(? symbol? _) (tea-symbol data)]
    [(list data ...)
     (tea-list (map parse-tea-data data))]
    [_ (error 'lexer "expected ~s to be a datum" exp)]))
