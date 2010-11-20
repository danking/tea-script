#lang racket
(require "../parser.rkt")
(require "../js-data.rkt")
(require "../scheme-to-js.rkt")
(require test-engine/racket-tests)

(define (to-js sexp) (tea-defexp->js (parse-tea-defexp sexp)))
(define sa string-append)

;; numbers
(check-expect (to-js '3) (jnumber 3))
(check-expect (to-js '-42) (jnumber -42))

;; id
(check-expect (to-js 'foo) (jid 'foo))
(check-expect (to-js 'foo-bar) '???)

;; strings
(check-expect (to-js '"foo") (jstring "foo"))
(check-expect (to-js '"foo-BAR_baz") (jstring "foo-BAR_baz"))

;; symbol
(check-expect (to-js ''foo) (jnew (jid '__tea_quote)
                                  (list (jstring "foo"))))
(check-expect (to-js ''bar-BAZ-qux) (jnew (jid '__tea_quote)
                                          (list (jstring "bar-BAZ-qux"))))

;; array
(check-expect (to-js ''(1 2 3))
              (jarray (list (jnumber 1)
                            (jnumber 2)
                            (jnumber 3))))
(check-expect (to-js ''(1 "a" b (1 3)))
              (jarray (list (jnumber 1)
                            (jstring "a")
                            (jnew (jid '__tea_quote)
                                  (list (jstring "b")))
                            (jarray (list (jnumber 1)
                                          (jnumber 3))))))

;; lambda
(check-expect (to-js '(lambda (x) x))
              (jlambda (list (jid 'x))
                       (list (jreturn (jid 'x)))))
(check-expect (to-js '(lambda (x y z) "foo"))
              (jlambda (list (jid 'x) (jid 'y) (jid 'z))
                       (list (jreturn (jstring "foo")))))

;; apply
(check-expect (to-js '(foo bar baz))
              (japply (jid 'foo)
                      (list (jid 'bar)
                            (jid 'baz))))
(check-expect (to-js '((foo bar) (bar baz) qux))
              (japply (japply (jid 'foo)
                              (list (jid 'bar)))
                      (list (japply (jid 'bar)
                                    (list (jid 'baz)))
                            (jid 'qux))))

;; variable definition
(check-expect (to-js '(define x 5))
              (jvdef (jid 'x) (jnumber 5)))
(check-expect (to-js '(define x (lambda (x) x)))
              (jvdef (jid 'x)
                     (jlambda (list (jid 'x))
                              (list (jreturn (jid 'x))))))

;; function definition
(check-expect (to-js '(define (foo x) x))
              (jfdef (jid 'foo)
                     (list (jid 'x))
                     (list (jreturn (jid 'x)))))
(check-expect (to-js '(define (foo x y z) (x y z)))
              (jfdef (jid 'foo)
                     (list (jid 'x)
                           (jid 'y)
                           (jid 'z))
                     (list (jreturn (japply (jid 'x)
                                            (list (jid 'y)
                                                  (jid 'z)))))))

(test)