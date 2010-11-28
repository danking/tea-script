#lang racket
(provide (except-out (all-defined-out) tstruct js-dispatcher))

(define-syntax tstruct
  (syntax-rules ()
    [(_ args ...) (struct args ... #:transparent)]))

(tstruct jstatement ())
  ;; (jdef JS-Id)
  (tstruct jdef jstatement (name))
    ;; (jvdef JId JExp)
    (tstruct jvdef jdef (exp))
    ;; (jfdef JId [ListOf JId] [ListOf JStatement])
    (tstruct jfdef jdef (args body))
  (tstruct jexp jstatement ())
    ;; (jid Symbol)
    (tstruct jid jexp (name))
    (tstruct jprim jexp ())
      ;; (jbool Boolean)
      (tstruct jbool jprim (value))
      ;; (jundef)
      (tstruct jundef jprim ())
      ;; (jnull)
      (tstruct jnull jprim ())
      ;; (jstring String)
      (tstruct jstring jprim (value))
      ;; (jnumber Number)
      (tstruct jnumber jprim (value))
      ;; (jarray [ListOf JExp])
      (tstruct jarray jprim (value))
    (tstruct jop jexp ())
      ;; (jnumop Symbol (U (list JExp) (list JExp JExp)))
      (tstruct jprimop jop (op args))
      ;; (jbracket JExp JString)
      (tstruct jbracket jop (object property))
      ;; (jcond JExp JExp JExp)
      (tstruct jcond jop (condition iftrue iffalse))
      ;; (jcomma [ListOf JExp])
      (tstruct jcomma jop (exps))
      ;; (jin JExp JExp)
      (tstruct jin jop (property object))
      ;; (jinstof JExp JExp
      (tstruct jinstof jop (object type))
      ;; (jtypeof JExp)
      (tstruct jtypeof jop (exp))
      ;; (jdot JExp JId)
      (tstruct jdot jop (object property))
      ;; (jnew JId [ListOf JExp])
      (tstruct jnew jop (name args))
    ;; (japply JExp [ListOf JExp])
    (tstruct japply jexp (exp args))
    ;; (jlambda [ListOf JId] [ListOf JStatement])
    (tstruct jlambda jexp (args body))
  ;; (jreturn JExp)
  (tstruct jreturn jstatement (exp))

;; matches value according to the specified rules, just like match, but if
;; one of the js structs is left unspecified it will default to recursively
;; rebuild itself transforming children as specified by exp-proc and id-proc
(define-syntax match-js
  (syntax-rules (match-js)
    ((match-js value exp-proc id-proc
       (match-exp result-exp) ...)
     (match value
       (match-exp result-exp) ...
       (_ (js-dispatcher value exp-proc id-proc))))))

;; js-dispatcher : JStatement [JExp -> JExp] [JId -> JId]
(define (js-dispatcher jstatement exp-proc id-proc)
  (define (dispatch-jprim jprim)
    (match jprim
      [(jarray v) (jarray (exp-proc v))]
      [_ jprim]))
  (define (dispatch-jop jop)
    (match jop
      [(jprimop op args) (jprimop op (map exp-proc args))]
      [(jbracket object property) (jbracket (exp-proc object)
                                            property)]
      [(jcond c t f) (jcond (exp-proc c)
                            (exp-proc t)
                            (exp-proc f))]
      [(jcomma exps) (jcomma (map exp-proc exps))]
      [(jin property object) (jin (exp-proc property)
                                  (exp-proc object))]
      [(jinstof object type) (jinstof (exp-proc object)
                                      (exp-proc type))]
      [(jtypeof exp) (jtypeof (exp-proc exp))]
      [(jdot object property) (jdot (exp-proc object)
                                    (exp-proc property))]
      [(jnew name args) (jnew (id-proc name)
                              (map exp-proc args))]))

  (match jstatement
    [(jvdef name exp) (jvdef (id-proc name)
                             (exp-proc exp))]
    [(jfdef name args body) (jfdef (id-proc name)
                                   (map id-proc args)
                                   (map exp-proc body))]
    [(jprim) (dispatch-jprim jstatement)]
    [(jid name) (id-proc jstatement)]
    [(jop) (dispatch-jop jstatement)]
    [(japply exp args) (japply (exp-proc exp)
                               (map exp-proc args))]
    [(jlambda args body) (jlambda (map id-proc args)
                                  (map exp-proc body))]
    [(jreturn exp) (jreturn (exp-proc exp))]))
