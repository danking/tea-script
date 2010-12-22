#lang racket
(provide make-symbol-unique)

;; make-symbol-unique : Symbol [ListOf Symbol] -> Symbol
(define (make-symbol-unique id used)
  (if (bad-id? id)
      (make-symbol-unique* (sanitize id) 0 used)
      id))

(define (make-symbol-unique* id i used)
  (let ([next-id (append-number id i)])
    (if (memq next-id used)
        (make-symbol-unique* id (add1 i) used)
        next-id)))

;; append-number : Symbol Number -> Symbol
(define (append-number id i)
  (string->symbol (string-append (symbol->string id)
                                 (number->string i))))

;; sanitize : Symbol -> Symbol
(define (sanitize id)
  (let ([chars (string->list (symbol->string id))])
    (string->symbol (list->string (map sanitize-char chars)))))

;; bad-id? : Symbol -> Boolean
(define (bad-id? id)
  (ormap (lambda (char) (assq char char-map))
         (string->list (symbol->string id))))

;; sanitize-char : Character -> Character
(define (sanitize-char char)
  (let ([mapping (assq char char-map)])
    (if mapping (cdr mapping) char)))

;; try to avoid a many-to-one relationship in char-map
(define char-map
  '([#\- . #\_]
    [#\+ . #\a]
    [#\/ . #\d]
    [#\* . #\m]
    [#\= . #\e]
    [#\& . #\n]
    [#\^ . #\c]
    [#\% . #\r]
    [#\# . #\h]
    [#\@ . #\t]
    [#\! . #\b]
    [#\~ . #\_]
    [#\. . #\_]
    [#\< . #\l]
    [#\> . #\g]
    [#\: . #\_]
    [#\? . #\p]))
