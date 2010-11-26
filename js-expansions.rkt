#lang racket
(require "js-data.rkt")
(provide expand-js)

(define (expand-js jstatement)
  (match-js jstatement expand-js (lambda (x) x)
    [(japply exp args) (expand-japply exp args)]))

(define (expand-jss jstatements)
  (map expand-js jstatements))

(define (expand-japply op args)
  (match op
    [(jid v) (match v
               [(or '! '++_ '--_ '_++ '_--)
                (if (singleton? args)
                    (jprimop v (expand-jss args))
                    (error 'expand-js "~a is a unary operator!" op))]
               [(or '/ '* '== '!= '=== '!== '> '>= '< '<= '&& '||)
                (if (2tuple? args)
                    (jprimop v (expand-jss args))
                    (error 'expand-js "~a is a binary operator!" op))]
               [(or '+ '-)
                (if (singleton-or-2tuple? args)
                    (jprimop v (expand-jss args))
                    (expand-plus/minus v (expand-jss args)))]
               [_ (japply op (expand-jss args))])]
    [_ (japply (expand-js op) (expand-jss args))]))

(define (expand-plus/minus op args)
  (if (empty? args)
      (if (symbol=? op '+) (jnumber 1) (jnumber -1))
      (foldl (lambda (arg args)
               (if (empty? args)
                   arg
                   (jprimop op (list args arg))))
             '()
             args)))

(define (singleton-or-2tuple? ls)
  (or (singleton? ls)
      (2tuple? ls)))

(define (singleton? ls)
  (and (not (empty? ls))
       (empty? (rest ls))))

(define (2tuple? ls)
  (and (not (empty? ls))
       (not (empty? (rest ls)))
       (empty? (rest (rest ls)))))