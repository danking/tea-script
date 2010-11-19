#lang racket
(require "js-data.scm")
(require "string-utilities.scm")
(provide jstatement->text
         formatting
         formatting-indent?
         formatting-depth)

(define INDENT_CHAR "  ")

(struct formatting (indent? depth))
;; A formatting is a (formatting Boolean Number)
;; where ...
;;  indent? indicates that this statement should begin with an indent
;;  depth   indicates how many levels deep the code is
(define (depth+1 fmt)
  (formatting (formatting-indent? fmt)
              (+ 1 (formatting-depth fmt))))

(define (jstatement->text js)
  (jstatement->text* js (formatting #f 0)))

(define (jstatement->text* js formatting)
  (indent formatting
          (string-append
           ((match js
              [(jdef name) jdef->text]
              [(jexp)      jexp->text]
              [(jreturn v) jreturn->text])
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
    [(jstring s) (string-append "\"" s "\"")]
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
    [(jdot object property) (format "~a.~a"
                                    (jexp->text object formatting)
                                    (jid->text property))]
    [(jlambda args body) (format (string-append "(function (~a) {\n"
                                                "~a"
                                                "})")
                                 (->csv jid->text args)
                                 (jstatements->text body formatting))]
    [(jnew name args) (format "new ~a(~a)"
                              (jid->text name)
                              (->csv (lambda (exp)
                                       (jexp->text exp formatting))
                                     args))]))

(define (jid->text my-jid)
  (match my-jid
    [(jid v) (symbol->string v)]
    [_ (error 'jid->text "expected an id, but got ~s" my-jid)]))

(define (jreturn->text jreturn formatting)
  (string-append "return " (jexp->text (jreturn-exp jreturn) formatting)))


(define (indent formatting s)
  (string-append (string-repeat (formatting-depth formatting)
                                INDENT_CHAR)
                 s))
