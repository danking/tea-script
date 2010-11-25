#lang racket
(require rackunit
         "../make-uid.rkt")

(define make-uid-ts
  (test-suite
   "tests for make-uid.rkt"
   (test-case
    "one frame"
    (check-equal? (sanitize-env '((foo-bar_ foo-bar-)))
                  '((foo_bar_0 foo_bar_1))))
   (test-case
    "two frames"
    (check-equal? (sanitize-env '((foo-bar_) (foo-bar-)))
                  '((foo_bar_0) (foo_bar_1)))
    (check-equal? (sanitize-env '((foo-bar_) (foo-bar_)))
                  '((foo_bar_0) (foo_bar_0))))
   ;; this test case depends on not changing "clean" ids, which currently
   ;; isn't supported because I haven't built the infrastructure
   ;; in sanitize-ids to properly work like this
   (test-case
    "three frames and clean ids"
    (check-equal? (sanitize-env '((foo_bar_)
                                  (foo-bar_)
                                  (foo_bar- foo_bar_ foo-bar_)))
                  '((foo_bar_)
                    (foo_bar_0)
                    (foo_bar_1 foo_bar_ foo_bar_0))))))

(require rackunit/text-ui)

(run-tests make-uid-ts)
