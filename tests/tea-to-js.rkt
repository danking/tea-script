#lang racket
(require "../js-data.rkt"
         "../parser.rkt"
         rackunit
         "../tea-to-js.rkt")

(define (to-js sexp) (tea-defexp->js (parse-tea-defexp sexp)))
(define sa string-append)

(define tea-to-js-ts
  (test-suite
   "tests for tea-to-js.rkt"
   (test-case
    "numbers"
    (check-equal? (to-js '3) (jnumber 3))
    (check-equal? (to-js '-42) (jnumber -42)))

   (test-case
    "id"
    (check-equal? (to-js 'foo) (jid 'foo)))

   (test-case
    "strings"
    (check-equal? (to-js '"foo") (jstring "foo"))
    (check-equal? (to-js '"foo-BAR_baz") (jstring "foo-BAR_baz")))

   (test-case
    "symbol"
    (check-equal? (to-js ''foo) (jnew (jid '__tea_quote)
                                      (list (jstring "foo"))))
    (check-equal? (to-js ''bar-BAZ-qux) (jnew (jid '__tea_quote)
                                              (list (jstring "bar-BAZ-qux")))))

   (test-case
    "array"
    (check-equal? (to-js ''(1 2 3))
                  (jarray (list (jnumber 1)
                                (jnumber 2)
                                (jnumber 3))))
    (check-equal? (to-js ''(1 "a" b (1 3)))
                  (jarray (list (jnumber 1)
                                (jstring "a")
                                (jnew (jid '__tea_quote)
                                      (list (jstring "b")))
                                (jarray (list (jnumber 1)
                                              (jnumber 3)))))))

   (test-case
    "lambda"
    (check-equal? (to-js '(lambda (x) x))
                  (jlambda (list (jid 'x))
                           (list (jreturn (jid 'x)))))
    (check-equal? (to-js '(lambda (x y z) "foo"))
                  (jlambda (list (jid 'x) (jid 'y) (jid 'z))
                           (list (jreturn (jstring "foo"))))))

   (test-case
    "if"
    (check-equal? (to-js '(if (even? x) (foo x) (bar x)))
                  (jcond (japply (jid 'even?)
                                 (list (jid 'x)))
                         (japply (jid 'foo)
                                 (list (jid 'x)))
                         (japply (jid 'bar)
                                 (list (jid 'x))))))

   (test-case
    "cond"
    (check-equal? (to-js '(cond [(even? x) (foo x)]
                                [(odd? x) (bar x)]
                                [else 0]))
                  (jcond (japply (jid 'even?)
                                 (list (jid 'x)))
                         (japply (jid 'foo)
                                 (list (jid 'x)))
                         (jcond (japply (jid 'odd?)
                                        (list (jid 'x)))
                                (japply (jid 'bar)
                                        (list (jid 'x)))
                                (jnumber 0))))
    (check-equal? (to-js '(cond [(even? x) 0]))
                  (jcond (japply (jid 'even?)
                                 (list (jid 'x)))
                         (jnumber 0)
                         (jnull)))
    (check-equal? (to-js '(cond))
                  (jnull)))

   (test-case
    "let"
    (check-equal? (to-js '(let ((foo 3) (bar "baz"))
                            (sum foo bar)))
                  (japply (jlambda (list (jid 'foo)
                                         (jid 'bar))
                                   (list (jreturn
                                          (japply (jid 'sum)
                                                  (list (jid 'foo)
                                                        (jid 'bar))))))
                          (list (jnumber 3) (jstring "baz")))))

   (test-case
    "let*"
    (check-equal? (to-js '(let* ((foo 3) (bar foo))
                            (sum foo bar)))
                  (japply (jlambda (list (jid 'foo))
                                   (list
                                    (jreturn
                                     (japply (jlambda (list (jid 'bar))
                                                      (list
                                                       (jreturn
                                                        (japply (jid 'sum)
                                                                (list
                                                                 (jid 'foo)
                                                                 (jid 'bar))))))
                                             (list (jid 'foo))))))
                          (list (jnumber 3)))))

   (test-case
    "letrec"
    (check-equal? (to-js '(letrec ((foo (lambda () foo)) (bar foo))
                            (bar)))
                  (japply (jlambda (list)
                                   (list (jvdef (jid 'foo)
                                                (jlambda (list)
                                                         (list
                                                          (jreturn
                                                           (jid 'foo)))))
                                         (jvdef (jid 'bar) (jid 'foo))
                                         (jreturn (japply (jid 'bar) (list)))))
                          (list))))

   (test-case
    "apply"
    (check-equal? (to-js '(foo bar baz))
                  (japply (jid 'foo)
                          (list (jid 'bar)
                                (jid 'baz))))
    (check-equal? (to-js '((foo bar) (bar baz) qux))
                  (japply (japply (jid 'foo)
                                  (list (jid 'bar)))
                          (list (japply (jid 'bar)
                                        (list (jid 'baz)))
                                (jid 'qux)))))

   (test-case
    "raise"
    (check-equal? (to-js '(raise "no good!"))
                  (jthrow (jstring "no good!"))))

   (test-case
    "variable definition"
    (check-equal? (to-js '(define x 5))
                  (jvdef (jid 'x) (jnumber 5)))
    (check-equal? (to-js '(define x (lambda (x) x)))
                  (jvdef (jid 'x)
                         (jlambda (list (jid 'x))
                                  (list (jreturn (jid 'x)))))))

   (test-case
    "function definition"
    (check-equal? (to-js '(define (foo x) x))
                  (jfdef (jid 'foo)
                         (list (jid 'x))
                         (list (jreturn (jid 'x)))))
    (check-equal? (to-js '(define (foo x y z) (x y z)))
                  (jfdef (jid 'foo)
                         (list (jid 'x)
                               (jid 'y)
                               (jid 'z))
                         (list (jreturn (japply (jid 'x)
                                                (list (jid 'y)
                                                      (jid 'z))))))))))

(require rackunit/text-ui)

(run-tests tea-to-js-ts)
