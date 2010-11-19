#lang scheme

(define (run x)
  (error 'run "not implemented!"))

;; symbols
(check-expect (run '(symbol=? foo-bar_ foo_bar-)) #f) ; depends on gensym
