#lang racket
(require "../tea-data.rkt"
         rackunit
         "../parser.rkt")

(define pt parse-tea-defexp)

(define parser-ts
  (test-suite
   "tests for the parser"
   (test-case
    "define statements"
    (check-equal? (pt '(define foo 3))
                  (tea-define (tea-id 'foo) (tea-number 3)))
    (check-equal? (pt '(define (foo bar baz) (+ bar baz)))
                  (tea-pdefine (tea-id 'foo)
                               (list (tea-id 'bar)
                                     (tea-id 'baz))
                               (list (tea-apply (tea-id '+)
                                                (list (tea-id 'bar)
                                                      (tea-id 'baz)))))))
   (test-case
    "self-evals"
    (check-equal? (pt '3)
                  (tea-number 3))
    (check-equal? (pt '"foo")
                  (tea-string "foo"))
    (check-equal? (pt 'foo)
                  (tea-id 'foo))
    (check-equal? (pt ''foo)
                  (tea-symbol 'foo)))
   (test-case
    "apply"
    (check-equal? (pt '(+ 1 foo))
                  (tea-apply (tea-id '+)
                             (list (tea-number 1)
                                   (tea-id 'foo)))))
   (test-case
    "lambda"
    (check-equal? (pt '(lambda (x y z) (- x y z)))
                  (tea-lambda (list (tea-id 'x)
                                    (tea-id 'y)
                                    (tea-id 'z))
                              (list (tea-apply (tea-id '-)
                                               (list (tea-id 'x)
                                                     (tea-id 'y)
                                                     (tea-id 'z)))))))
   (test-case
    "if"
    (check-equal? (pt '(if x (+ 3 6) (y 4)))
                  (tea-if (tea-id 'x)
                          (tea-apply (tea-id '+)
                                     (list (tea-number 3)
                                           (tea-number 6)))
                          (tea-apply (tea-id 'y)
                                     (list (tea-number 4))))))
   (test-case
    "cond"
    (check-equal? (pt '(cond [(foo? bar) (+ baz 3)]
                             [(qux? quux) (string-append quux "la-te-dah")]
                             [else 'else!]))
                  (tea-cond (list (tea-apply (tea-id 'foo?)
                                             (list (tea-id 'bar)))
                                  (tea-apply (tea-id 'qux?)
                                             (list (tea-id 'quux))))
                            (list (tea-apply (tea-id '+)
                                             (list (tea-id 'baz)
                                                   (tea-number 3)))
                                  (tea-apply (tea-id 'string-append)
                                             (list (tea-id 'quux)
                                                   (tea-string "la-te-dah"))))
                            (tea-symbol 'else!)))
    (check-equal? (pt '(cond [(foo? bar) (+ baz 3)]
                             [(qux? quux) (string-append quux "la-te-dah")]))
                  (tea-cond (list (tea-apply (tea-id 'foo?)
                                             (list (tea-id 'bar)))
                                  (tea-apply (tea-id 'qux?)
                                             (list (tea-id 'quux))))
                            (list (tea-apply (tea-id '+)
                                             (list (tea-id 'baz)
                                                   (tea-number 3)))
                                  (tea-apply (tea-id 'string-append)
                                             (list (tea-id 'quux)
                                                   (tea-string "la-te-dah"))))
                            (tea-void)))
        (check-equal? (pt '(cond))
                      (tea-cond (list) (list) (tea-void))))
   (test-case
    "let"
    (check-equal? (pt '(let [(x 3) (y 4)]
                         (+ x y)))
                  (tea-let (list (tea-id 'x) (tea-id 'y))
                           (list (tea-number 3) (tea-number 4))
                           (list (tea-apply (tea-id '+)
                                            (list (tea-id 'x)
                                                  (tea-id 'y)))))))
   (test-case
    "let*"
    (check-equal? (pt '(let* [(x 3) (y 4)]
                         (+ x y)))
                  (tea-let* (list (tea-id 'x) (tea-id 'y))
                            (list (tea-number 3) (tea-number 4))
                            (list (tea-apply (tea-id '+)
                                             (list (tea-id 'x)
                                                   (tea-id 'y)))))))
   (test-case
    "letrec"
    (check-equal? (pt '(letrec [(x 3) (y 4)]
                         (+ x y)))
                  (tea-letrec (list (tea-id 'x) (tea-id 'y))
                              (list (tea-number 3) (tea-number 4))
                              (list (tea-apply (tea-id '+)
                                               (list (tea-id 'x)
                                                     (tea-id 'y)))))))
   (test-case
    "quoted lists"
    (check-equal? (pt ''(1 2 (3 4 (5 6) a b) "c" "d"))
                  (tea-list (list (tea-number 1)
                                  (tea-number 2)
                                  (tea-list (list (tea-number 3)
                                                  (tea-number 4)
                                                  (tea-list (list
                                                             (tea-number 5)
                                                             (tea-number 6)))
                                                  (tea-symbol 'a)
                                                  (tea-symbol 'b)))
                                  (tea-string "c")
                                  (tea-string "d")))))
   (test-case
    "raise"
    (check-equal? (pt '(raise 3))
                  (tea-raise (tea-number 3))))
   (test-case
    "send"
    (check-equal? (pt '(send foo bar))
                  (tea-send (tea-id 'foo)
                            (tea-id 'bar)
                            (list)))
    (check-equal? (pt '(send foo bar 3 4))
                  (tea-send (tea-id 'foo)
                            (tea-id 'bar)
                            (list (tea-number 3)
                                  (tea-number 4)))))
   (test-case
    "get-field"
    (check-equal? (pt '(get-field foo bar))
                  (tea-get-field (tea-id 'foo)
                                 (tea-id 'bar)))
    (check-equal? (pt '(get-field foo bar-baz))
                  (tea-get-field (tea-id 'foo)
                                 (tea-id 'bar-baz)))
    (check-equal? (pt '(get-field foo "bar-baz"))
                  (tea-get-field (tea-id 'foo)
                                 (tea-string "bar-baz"))))))

(require rackunit/text-ui)

(run-tests parser-ts)