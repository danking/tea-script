#lang racket
(require "../js-data.scm")
(require rackunit "../js-to-text.scm")

(define sa string-append)
(define jt jstatement->text)

(define js-to-text-ts
  (test-suite
   "Tests for js-to-text.scm"
   (test-case
    "JS Variable Definitions"
    (check-equal? (jt (jvdef (jid 'x)
                             (jid 'y)))
                  "var x = y;\n")
    (check-equal? (jt (jvdef (jid 'x)
                             (jnumber 3)))
                  "var x = 3;\n"))
   (test-case
    "JS Function Definitions"
    (check-equal? (jt (jfdef (jid 'foo)
                             (list (jid 'x))
                             (list (jid 'x))))
                  (sa "function foo (x) {\n"
                      "  x;\n"
                      "};\n"))
    (check-equal? (jt (jfdef (jid 'bar)
                             (list (jid 'x))
                             (list (jreturn (jid 'x)))))
                  (sa "function bar (x) {\n"
                      "  return x;\n"
                      "};\n"))
    (check-equal? (jt (jfdef (jid 'baz)
                             (list (jid 'x)
                                   (jid 'y)
                                   (jid 'z))
                             (list (jid 'y)
                                   (jreturn (jid 'x)))))
                  (sa "function baz (x, y, z) {\n"
                      "  y;\n"
                      "  return x;\n"
                      "};\n"))
    (check-equal? (jt (jfdef (jid 'foo-bar_)
                             (list (jid 'x))
                             (list (jreturn (jid 'x)))))
                  '???))
   (test-case
    "JS Identifiers"
    (check-equal? (jt (jid 'x))
                  "x;\n")
    (check-equal? (jt (jid 'foo_bar))
                  "foo_bar;\n")
    (check-equal? (jt (jid 'foo-bar_))
                  '???)
    (check-not-equal? (jt (jid 'foo-bar_))
                      (jt (jid 'foo_bar-))))
   (test-case
    "JS Bool"
    (check-equal? (jt (jbool true))
                  "true;\n")
    (check-equal? (jt (jbool false))
                  "false;\n"))
   (test-case
    "JS Undef"
    (check-equal? (jt (jundef))
                  "undefined;\n"))
   (test-case
    "JS Null"
    (check-equal? (jt (jnull))
                  "null;\n"))
   (test-case
    "JS String"
    (check-equal? (jt (jstring "foo bar baz"))
                  "\"foo bar baz\";\n"))
   (test-case
    "JS Number"
    (check-equal? (jt (jnumber 3))
                  "3;\n")
    (check-equal? (jt (jnumber (expt 2 53))) ; max JS int value
                  "9007199254740992;\n")
    (check-equal? (jt (jnumber (expt 10 309))) ; 10^308 is JS max number
                  '?????))
   (test-case
    "JS Array"
    (check-equal? (jt (jarray (list (jnumber 1)
                                    (jnumber 2)
                                    (jnumber 3))))
                  "[1, 2, 3];\n")
    (check-equal? (jt (jarray (list (jnumber 1)
                                    (jnumber 2)
                                    (jid 'a)
                                    (jid 'b)
                                    (jstring "c")
                                    (jstring "d"))))
                  "[1, 2, a, b, \"c\", \"d\"];\n"))
   (test-case
    "JS Application"
    (check-equal? (jt (japply (jid 'foo) (list (jid 'bar))))
                  "foo(bar);\n")
    (check-equal? (jt (japply (jid 'foo) (list (jid 'bar)
                                               (jid 'baz)
                                               (jid 'qux))))
                  "foo(bar, baz, qux);\n")
    (check-equal? (jt (japply (jlambda
                               (list (jid 'x)
                                     (jid 'y))
                               (list (jreturn (japply (jid 'foo)
                                                      (list (jid 'x)
                                                            (jid 'y))))))
                              (list (jnumber 3) (jstring "bar"))))
                  (sa "(function (x, y) {\n"
                      "  return foo(x, y);\n"
                      "})(3, \"bar\");\n"))
    (check-equal? (jt (jlambda (list (jid 'x))
                               (list (jreturn (jid 'x)))))
                  (sa "(function (x) {\n"
                      "  return x;\n"
                      "});\n")))
   (test-case
    "JS Dot Operator"
    (check-equal? (jt (jdot (jid 'foo) (jid 'bar)))
                  "foo.bar;\n")
    (check-equal? (jt (jdot (jdot (jid 'foo) (jid 'bar))
                            (jid 'baz)))
                  "foo.bar.baz;\n")
    (check-equal? (jt (japply (jdot (jid 'foo) (jid 'bar))
                              (list)))
                  "foo.bar();\n")
    (check-equal? (jt (japply (jdot (japply (jdot (jid 'foo)
                                                  (jid 'bar))
                                            (list))
                                    (jid 'baz))
                              (list)))
                  "foo.bar().baz();\n"))
   (test-case
    "JS Lambda"
    (check-equal? (jt (jlambda (list) (list)))
                  (sa "(function () {\n"
                      "});\n"))
    (check-equal? (jt (jlambda (list (jid 'x))
                               (list (jid 'x))))
                  (sa "(function (x) {\n"
                      "  x;\n"
                      "});\n"))
    (check-equal? (jt (jlambda (list (jid 'x)
                                     (jid 'y))
                               (list (jreturn
                                      (japply (jid 'foo)
                                              (list (jid 'x)
                                                    (jid 'y)))))))
                  (sa "(function (x, y) {\n"
                      "  return foo(x, y);\n"
                      "});\n")))
   (test-case
    "JS New"
    (check-equal? (jt (jnew (jid 'Object) (list)))
                  "new Object();\n")
    (check-equal? (jt (jnew (jid 'Object) (list (jid 'x)
                                                (jnumber 3)
                                                (jstring "foo"))))
                  "new Object(x, 3, \"foo\");\n"))
   (test-case
    "JS Return"
    (check-equal? (jt (jreturn (jid 'foo)))
                  "return foo;\n")
    (check-equal? (jt (jreturn (jlambda (list (jid 'x))
                                        (list (jreturn (jid 'bar))))))
                  (sa "return (function (x) {\n"
                      "  return bar;\n"
                      "});\n")))))

(require rackunit/text-ui)

(run-tests js-to-text-ts)