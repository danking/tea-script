#lang racket
(require "js-data.rkt")
(require "string-utilities.rkt")
(provide jstatement->text
         formatting
         formatting-depth
         formatting-indent-str)

(struct formatting (depth indent-str))
;; A formatting is a (formatting Boolean Number)
;; where ...
;;  depth indicates how many levels deep the code is
;;  indent-str the string used to indent code
(define (depth+1 fmt)
  (formatting (+ 1 (formatting-depth fmt))
              (formatting-indent-str fmt)))

(define (jstatement->text js)
  (jstatement->text* js (formatting 0 "  ")))

(define (jstatement->text* js formatting)
  (indent formatting
          (string-append
           ((match js
              [(jdef name) jdef->text]
              [(jexp)      jexp->text]
              [(jreturn v) jreturn->text]
              [(jthrow v)  jthrow->text])
            js formatting)
           ";\n")))

(define (jstatements->text jss formatting)
  (foldl (lambda (s ss)
           (string-append ss
                          (jstatement->text* s (depth+1 formatting))))
         ""
         jss))

(define (jdef->text jdef formatting)
  (match jdef
    [(jvdef id value)
     (format "var ~a = ~a"
             (jid->text id)
             (jexp->text value formatting))]
    [(jfdef id args body)
     (format (string-append "function ~a (~a) {\n"
                            "~a"
                            "}")
             (jid->text id)
             (->csv jid->text args)
             (jstatements->text body formatting))]))

(define (jexp->text jexp formatting)
  (match jexp
    [(jid id)    (jid->text jexp)]  ; fixme: deal with js unfriendly ids
    [(jbool b)   (if b "true" "false")]
    [(jundef)    "undefined"]
    [(jnull)     "null"]
    [(jstring s) (jstring->text jexp)]
    [(jnumber n) (number->string n)] ; fixme: use BigNum instead
    [(jarray ls) (string-append "[" (->csv (lambda (exp)
                                             (jexp->text exp formatting))
                                           ls)
                                "]")]
    [(japply exp args) (format "~a(~a)"
                               (jexp->text exp formatting)
                               (->csv (lambda (exp)
                                        (jexp->text exp formatting))
                                      args))]
    [(jlambda args body) (format (string-append "(function (~a) {\n"
                                                "~a"
                                                "})")
                                 (->csv jid->text args)
                                 (jstatements->text body formatting))]
    [(jop) (jop->text jexp formatting)]))

(define (jop->text jop formatting)
  (match jop
    [(jprimop op args) (jprimop->text jop formatting)]
    [(jbracket object property) (format "~a[~a]"
                                        (jexp->text object formatting)
                                        (jstring->text property))]
    [(jcond c t f) (format "(~a ? ~a : ~a)"
                           (jexp->text c formatting)
                           (jexp->text t formatting)
                           (jexp->text f formatting))]
    [(jcomma exps) (string-append "("
                                  (->csv (lambda (exp)
                                           (jexp->text exp formatting))
                                         exps)
                                  ")")]
    [(jin property object) (format "(~a in ~a)"
                                   (jexp->text property formatting)
                                   (jexp->text object formatting))]
    [(jinstof object type) (format "(~a instanceOf ~a)"
                                   (jexp->text object formatting)
                                   (jexp->text type formatting))]
    [(jtypeof exp) (format "(typeof ~a)" (jexp->text exp formatting))]
    [(jnew name args) (format "new ~a(~a)"
                              (jid->text name)
                              (->csv (lambda (exp)
                                       (jexp->text exp formatting))
                                     args))]
    [(jdot object property) (format "~a.~a"
                                    (jexp->text object formatting)
                                    (jid->text property))]))
(define (jprimop->text jprimop formatting)
  (string-append "(" (jprimop->text/noparens jprimop formatting) ")"))

(define (jprimop->text/noparens jprimop formatting)
  (let ([op (jprimop-op jprimop)]
        [args (map (lambda (exp) (jexp->text exp formatting))
                   (jprimop-args jprimop))])
    (match op
      [(or '+ '-)     (un/bin-ary->text op args)]
      [(or '/ '* '== '!= '=== '!== '> '>= '< '<= '&& '||)
       (binary->text op (first args) (second args))]
      ['!             (unary->text op (first args))]
      [(or '++_ '--_) (unary->text (substring (symbol->string op) 0 2)
                                   (first args))]
      [(or '_++ '_--) (unary->text (first args)
                                   (substring (symbol->string op) 1 3))])))

(define (un/bin-ary->text op args)
  (if (empty? (rest args))
      (unary->text op (first args))
      (binary->text op (first args) (second args))))

(define (unary->text head rear)
  (format "~a~a" head rear))

(define (binary->text op v1 v2)
  (format "~a ~a ~a" v1 op v2))

(define (jid->text my-jid)
  (match my-jid
    [(jid v) (symbol->string v)]
    [_ (error 'jid->text "expected an id, but got ~s" my-jid)]))

(define (jstring->text my-jstring) ; tee hee
  (match my-jstring
    [(jstring s) (string-append "\"" s "\"")]
    [_ (error 'jstring->text "expected a string, but got ~s" my-jstring)]))

(define (jreturn->text jreturn formatting)
  (string-append "return " (jexp->text (jreturn-exp jreturn) formatting)))

(define (jthrow->text jthrow formatting)
  (string-append "throw " (jexp->text (jthrow-exp jthrow) formatting)))

(define (indent formatting s)
  (string-append (string-repeat (formatting-depth formatting)
                                (formatting-indent-str formatting))
                 s))
