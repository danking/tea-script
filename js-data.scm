#lang racket
(provide (all-defined-out))

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
      (tstruct jundef jprim ())
      (tstruct jnull jprim ())
      ;; (jstring String)
      (tstruct jstring jprim (value))
      ;; (jnumber Number)
      (tstruct jnumber jprim (value))
      ;; (jarray [ListOf JExp])
      (tstruct jarray jprim (value))
    ;; (japply JExp [ListOf JExp])
    (tstruct japply jexp (exp args))
    ;; (jdot JExp JId)
    (tstruct jdot jexp (object property))
    ;; (jlambda [ListOf JId] [ListOf JStatement])
    (tstruct jlambda jexp (args body))
    ;; (jnew JId [ListOf JExp])
    (tstruct jnew jexp (name args))
  ;; (jreturn JExp)
  (tstruct jreturn jstatement (exp))
