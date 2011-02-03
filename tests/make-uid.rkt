#lang racket
(require rackunit
         "../make-uid.rkt")
(provide make-uid-ts)

(define make-uid-ts
  (test-suite
   "tests for make-uid.rkt"
   (test-case
    "clean symbols"
    (check-equal? (make-symbol-unique 'foo (list 'foo 'bar))
                  'foo)
    (check-equal? (make-symbol-unique 'bar (list 'foo 'bar))
                  'bar)
    (check-equal? (make-symbol-unique 'baz (list 'foo 'bar))
                  'baz))
   (test-case
    "dirty symbols"
    (check-equal? (make-symbol-unique 'foo- (list 'foo_))
                  'foo_0)
    (check-equal? (make-symbol-unique 'foo- (list 'foo_ 'foo_0))
                  'foo_1))))
