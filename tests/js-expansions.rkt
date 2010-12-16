#lang racket
(require "../js-data.rkt"
         "../parser.rkt"
         "../tea-to-js.rkt"
         rackunit
         "../js-expansions.rkt")

(define (pe sexp) (expand-js (tea-defexp->js (parse-tea-defexp sexp))))

(define-syntax check-no-change
  (syntax-rules ()
    [(_ sexp ...) (begin (check-equal?
                          (pe sexp)
                          (tea-defexp->js (parse-tea-defexp sexp))) ...)]))

(define-syntax check-unary-ops
  (syntax-rules ()
    [(_ op ...) (begin (check-equal? (pe `(,op 3))
                                     (jprimop op
                                              (list (jnumber 3)))) ...)]))

(define-syntax check-binary-ops
  (syntax-rules ()
    [(_ op ...) (begin (check-equal? (pe `(,op 3 4))
                                     (jprimop op
                                              (list (jnumber 3)
                                                    (jnumber 4)))) ...)]))

(define-syntax check-nary-ops
  (syntax-rules ()
    [(_ op ...) (begin
                  (begin (check-equal? (pe `(,op 1 2 3))
                                       (jprimop
                                        op
                                        (list (jprimop
                                               op
                                               (list (jnumber 1)
                                                     (jnumber 2)))
                                              (jnumber 3))))
                         (check-equal? (pe `(,op 1 2 3 4))
                                       (jprimop
                                        op
                                        (list (jprimop
                                               op
                                               (list (jprimop
                                                      op
                                                      (list (jnumber 1)
                                                            (jnumber 2)))
                                                     (jnumber 3)))
                                              (jnumber 4)))))
                  ...)]))

(define js-expansions-ts
 (test-suite
  "JS Expansions"
  (test-case
   "self-evals"
   (check-no-change 'a
                    "foo"
                    3
                    ''a
                    ''(1 2 3)))
  (test-case
   "language forms"
   (check-no-change '(define x 3)
                    '(define (f x) x)
                    '(let ((z 4)) z)
                    '(lambda (y) y)
                    '(foo bar baz)))
  (test-case
   "nullary operators"
   (check-equal? (pe '(+))
                 (jnumber 1))
   (check-equal? (pe '(-))
                 (jnumber -1)))
  (test-case
   "unary operators"
   (check-unary-ops '+ '- '! '++_ '--_ '_++ '_--))
  (test-case
   "binary operators"
   (check-binary-ops '+ '- '/ '* '== '!= '=== '!== '> '>= '< '<= '&& '||))
  (test-case
   "n-ary operators"
   (check-nary-ops '+ '-))
  (test-case
   "recursive case"
   (check-equal? (pe '(lambda (x y) (+ x y 2)))
                 (jlambda (list (jid 'x) (jid 'y))
                          (list (jreturn (jprimop
                                          '+
                                          (list (jprimop
                                                 '+
                                                 (list (jid 'x)
                                                       (jid 'y)))
                                                (jnumber 2))))))))))

(require rackunit/text-ui)

(run-tests js-expansions-ts)
