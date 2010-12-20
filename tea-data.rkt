#lang racket
(provide (except-out (all-defined-out) tstruct tea-dispatcher))

(define-syntax tstruct
  (syntax-rules ()
    [(_ args ...) (struct args ... #:transparent)]))

(tstruct tea-define (name value))
(tstruct tea-pdefine (name ids body))
(tstruct tea-symbol (value))
(tstruct tea-number (value))
(tstruct tea-string (value))
(tstruct tea-lambda (args body))
(tstruct tea-if     (c t f))
(tstruct tea-cond   (preds exps else))
(tstruct tea-let    (vars vals body))
(tstruct tea-let*   (vars vals body))
(tstruct tea-letrec (vars vals body))
(tstruct tea-apply  (head tail))
(tstruct tea-id     (value))
(tstruct tea-list   (value))
(tstruct tea-raise  (value))
(tstruct tea-void   ())
(tstruct tea-send   (object method))

;; a Tea-Define is a (tea-define Tea-Identifier Tea-Expression)

;; a Tea-Proc-Define is a (tea-proc-define Tea-Identifier
;;                                         [ListOf Tea-Identifier]
;;                                         [ListOf Tea-Expression])

;; a Tea-Symbol is a (tea-symbol Symbol)

;; a Tea-Number is a (tea-number Number)

;; a Tea-String is a (tea-string String)

;; a Tea-Lambda is a (tea-lambda [ListOf Tea-Identifier]
;;                               [ListOf Tea-Expression])

;; a Tea-If     is a (tea-if Tea-Expression Tea-Expression Tea-Expression)

;; a Tea-Cond   is a (tea-cond [ListOf Tea-Expression]
;;                             [ListOf Tea-Expression]
;;                             Tea-Expression)

;; a Tea-Let    is a (tea-let [ListOf Tea-Identifier]
;;                            [ListOf Tea-Expression]
;;                            [ListOf Tea-Expression])

;; a Tea-Apply  is a (tea-apply Tea-Expression
;;                              [ListOf Tea-Expression])

;; a Tea-Identifier is a (tea-identifier Symbol)

;; a Tea-List   is a (tea-list [ListOf Tea-Expression])

;; a Tea-Void   is a (tea-void)

(define-syntax match-tea
  (syntax-rules (match-tea)
    ((match-tea value exp-proc id-proc
       (match-exp result-exp) ...)
     (match value
       (match-exp result-exp) ...
       (_ (tea-dispatcher value exp-proc id-proc))))))

;; tea-dispatcher :
;;   Tea-Expression [Tea-Expression -> Tea-Expression] [Tea-Id -> Tea-Id]
(define (tea-dispatcher t exp-proc id-proc)
  (match t
    [(tea-define name value) (tea-define (id-proc name)
                                         (exp-proc value))]
    [(tea-pdefine name ids body) (tea-pdefine (id-proc name)
                                              (map id-proc ids)
                                              (map exp-proc body))]
    [(or (tea-symbol value)
         (tea-number value)
         (tea-string value)) t]
    [(tea-lambda args body) (tea-lambda (map id-proc args)
                                        (map exp-proc body))]
    [(tea-if c t f) (tea-if (exp-proc c)
                            (exp-proc t)
                            (exp-proc f))]
    [(tea-cond preds exps else) (tea-cond (map exp-proc preds)
                                          (map exp-proc exps)
                                          (exp-proc else))]
    [(tea-let vars vals body) (tea-let (map id-proc vars)
                                       (map exp-proc vals)
                                       (map exp-proc body))]
    [(tea-let* vars vals body) (tea-let* (map id-proc vars)
                                         (map exp-proc vals)
                                         (map exp-proc body))]
    [(tea-letrec vars vals body) (tea-letrec (map id-proc vars)
                                             (map exp-proc vals)
                                             (map exp-proc body))]
    [(tea-apply head tail) (tea-apply (exp-proc head)
                                      (map exp-proc tail))]
    [(tea-id value) (id-proc t)]
    [(tea-list value) (tea-list (map exp-proc value))]
    [(tea-raise value) (tea-raise (exp-proc value))]
    [(tea-void) t]
    [(tea-send object method) (tea-send (exp-proc object)
                                        (exp-proc method))]))

(define (tea-accumulator t accumulator exp-proc id-proc)
  (match t
    [(tea-define name value) (accumulator (list (id-proc name)
                                                (exp-proc value)))]
    [(tea-pdefine name ids body) (accumulator (cons (id-proc name)
                                                    (append
                                                     (map id-proc ids)
                                                     (map exp-proc body))))]
    [(or (tea-symbol value)
         (tea-number value)
         (tea-string value)) (accumulator '())]
    [(tea-lambda args body) (accumulator (append (map id-proc args)
                                                 (map exp-proc body)))]
    [(tea-if c t f) (accumulator (list (exp-proc c)
                                       (exp-proc t)
                                       (exp-proc f)))]
    [(tea-cond preds exps else) (accumulator (list (exp-proc else)
                                                   (append
                                                    (map exp-proc preds)
                                                    (map exp-proc exps))))]
    [(tea-let vars vals body) (accumulator (append (map id-proc vars)
                                                   (map exp-proc vals)
                                                   (map exp-proc body)))]
    [(tea-let* vars vals body) (accumulator (append (map id-proc vars)
                                                    (map exp-proc vals)
                                                    (map exp-proc body)))]
    [(tea-letrec vars vals body) (accumulator (append (map id-proc vars)
                                                      (map exp-proc vals)
                                                      (map exp-proc body)))]
    [(tea-apply head tail) (accumulator (list* (exp-proc head)
                                               (map exp-proc tail)))]
    [(tea-id value) (accumulator (list (id-proc t)))]
    [(tea-list value) (accumulator (map exp-proc value))]
    [(tea-raise value) (accumulator (list (exp-proc value)))]
    [(tea-void) (accumulator '())]
    [(tea-send object method) (accumulator (list (exp-proc object)
                                                 (exp-proc method)))]))
