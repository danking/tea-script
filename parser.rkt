#lang racket
(require "tea-data.rkt")
(provide parse-tea-defexp)

(define (parse-tea-defexp defexp)
  (match defexp
    [(list 'define _ ..1) (parse-tea-def defexp)]
    [_                    (parse-tea-exp defexp)]))

(define (parse-tea-def def)
  (match def
    [(list 'define (list name ids ...) bodies ..1)
     (tea-pdefine (parse-tea-id name)
                  (parse-tea-ids ids)
                  (map parse-tea-defexp bodies))]
    [(list 'define id body) (tea-define (parse-tea-id id)
                                        (parse-tea-exp body))]
    [_ (error 'parser "expected ~s to be a definition")]))

(define (parse-tea-exps exps)
  (map parse-tea-exp exps))

(define (parse-tea-exp exp)
  (match exp
    [(? string? _) (tea-string exp)]
    [(? number? _) (tea-number exp)]
    [(? symbol? _) (tea-id exp)]
    [(list 'quote (? symbol? s)) (tea-symbol s)]
    [(list 'quote ls) (tea-list (parse-tea-data ls))]
    [(list 'quasiquote ls) (error 'parser "quasiquote is not supported yet")]
    [(list 'lambda (list ids ...) bodies ..1)
     (tea-lambda (parse-tea-ids ids)
                 (parse-tea-exps bodies))]
    [(list 'if c t f)
     (tea-if (parse-tea-exp c)
             (parse-tea-exp t)
             (parse-tea-exp f))]
    [(list 'cond (list preds exps) ...)
     (parse-tea-cond preds exps)]
    [(list (and type (or 'let 'let* 'letrec))
           (list (list ids vals) ...)
           body ...)
     ((cond [(eq? type 'let) tea-let]
            [(eq? type 'let*) tea-let*]
            [(eq? type 'letrec) tea-letrec])
      (parse-tea-ids ids)
      (parse-tea-exps vals)
      (parse-tea-exps body))]
    [(list 'raise value)
     (tea-raise (parse-tea-exp value))]
    [(list 'send object method args ...)
     (tea-send (parse-tea-exp object)
               (parse-tea-id method)
               (parse-tea-exps args))]
    [(list 'get-field object field)
     (tea-get-field (parse-tea-exp object)
                    (parse-tea-exp field))]
    [(list head tail ...)
     (tea-apply  (parse-tea-exp head)
                 (parse-tea-exps tail))]
    [_ (error 'parser "expected ~s to be an expression" exp)]))

(define (parse-tea-ids ids)
  (map parse-tea-id ids))

(define (parse-tea-id id)
  (if (symbol? id) (tea-id id)
      (error 'lexer "expected ~s to be an identifier" id)))

(define (parse-tea-cond preds exps)
  (if (and (not (empty? preds))
           (eq? 'else (last preds)))
      (tea-cond (parse-tea-exps (all-but-last preds))
                (parse-tea-exps (all-but-last exps))
                (parse-tea-exp (last exps)))
      (tea-cond (parse-tea-exps preds)
                (parse-tea-exps exps)
                (tea-void))))

(define (all-but-last ls)
  (if (empty? (rest ls))
      '()
      (cons (first ls)
            (all-but-last (rest ls)))))

(define (parse-tea-data data)
  (map parse-tea-datum data))

(define (parse-tea-datum datum)
  (match datum
    [(? string? _) (tea-string datum)]
    [(? number? _) (tea-number datum)]
    [(? symbol? _) (tea-symbol datum)]
    [(list data ...)
     (tea-list (parse-tea-data data))]
    [_ (error 'lexer "expected ~s to be a datum" datum)]))
