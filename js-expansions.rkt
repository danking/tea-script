#lang racket
(require "js-data.rkt")
(provide expand-js)

(define (expand-js jstatement)
  (match jstatement
    [(jvdef name exp) (jvdef (expand-jid name)
                             (expand-js exp))]
    [(jfdef name args body) (jfdef (expand-jid name)
                                   (expand-jids args)
                                   (expand-jss body))]
    [(jprim) (expand-jprim jstatement)]
    [(jid name) (expand-jid& name)]
    [(jop) (expand-jop jstatement)]
    [(japply exp args) (expand-japply& exp args)]
    [(jlambda args body) (jlambda (expand-jids args)
                                  (expand-jss body))]
    [(jreturn exp) (jreturn (expand-js exp))]))

(define (expand-jss jstatements)
  (map expand-js jstatements))

(define (expand-jprim jprim)
  (match jprim
    [(jarray v) (jarray (expand-jss v))]
    [_ jprim]))

(define (expand-jop jop)
  (match jop
    [(jprimop op args) (jprimop op (expand-jss args))]
    [(jbracket object property) (jbracket (expand-js object)
                                          (expand-jprim property))]
    [(jcond c t f) (jprimop (expand-js c)
                            (expand-js t)
                            (expand-js f))]
    [(jcomma exps) (jcomma (expand-jss exps))]
    [(jin property object) (jin (expand-js property)
                                (expand-js object))]
    [(jinstof object type) (jinstof (expand-js object)
                                    (expand-js type))]
    [(jtypeof exp) (jtypeof (expand-js exp))]
    [(jdot object property) (jdot (expand-js object)
                                  (expand-js property))]
    [(jnew name args) (jnew (expand-jid name)
                            (expand-jss args))]))
(define (expand-jids jids)
  (map expand-jid jids))

(define (expand-jid my-jid)
  (match my-jid
    [(jid name) (expand-jid& name)]
    [_ (error 'expand-js "expected a jid got ~a" my-jid)]))

(define (expand-jid& name)
  (jid name))

(define (expand-japply my-japply)
  (match my-japply
    [(japply op args) (expand-japply& op args)]
    [_ (error 'expand-js "expected a japply got ~a" my-japply)]))

(define (expand-japply& op args)
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
               [_ (japply (expand-jid op) (expand-jss args))])]
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