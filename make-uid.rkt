#lang racket
(provide sanitize-env
         make-id-unique)

;; a Frame is a non-empty [ListOf Symbol]

;; an [AList X X] is a [ListOf (cons X X)]

(define (sanitize-env env)
  (accum (lambda (old-frame id-map)
           (let ([new-frame (sanitize-frame old-frame id-map)])
             (values new-frame
                     (append (map cons old-frame new-frame) id-map))))
         '()
         env))

;; make-uid/frame : Frame [AList Symbol Symbol] -> Frame
(define (sanitize-frame frame id-map)
  (accum (lambda (old-id id-map)
           (let ([new-id (make-id-unique old-id id-map)])
             (values new-id
                     (cons (cons old-id new-id) id-map))))
         id-map
         frame))

(define (make-id-unique id id-map)
  (if #t ; (bad-id? id)
      (let ([id&uid (assq id id-map)])
        (if id&uid
            (cdr id&uid)
            (make-id-unique* (sanitize id) 0 (map cdr id-map))))
      id))

;; make-id-unique* : Symbol Number [ListOf Symbol] -> Symbol
(define (make-id-unique* id i used)
  (let ([next-id (append-number id i)])
    (if (memq next-id used)
        (make-id-unique* id (add1 i) used)
        next-id)))

(define (append-number id i)
  (string->symbol (string-append (symbol->string id)
                                 (number->string i))))

;; sanitize : Symbol -> Symbol
(define (sanitize id)
  (let ([chars (string->list (symbol->string id))])
    (string->symbol (list->string (map sanitize-char chars)))))

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

;; bad-id? : Symbol -> Boolean
(define (bad-id? str)
  (ormap (lambda (char) (assq char char-map))
         (string->list (symbol->string str))))

(define (sanitize-char char)
  (let ([mapping (assq char char-map)])
    (if mapping (cdr mapping) char)))

;; (define (accum map-proc acc-proc acc ls)
;;   (let loop ([acc acc] [ls ls])
;;     (if (null? ls)
;;       '()
;;       (cons (map-proc (first ls) acc)
;;             (loop (acc-proc acc (first ls)) (rest ls))))))

;; (define (accum proc acc ls)
;;   (let loop ([acc acc] [ls ls])
;;     (if (empty? ls)
;;       '()
;;       (let-values ([(next-element next-acc) (proc (first ls) acc)])
;;         (cons next-element (loop next-acc (rest ls)))))))

(define (stateful-proc state proc)
  (lambda (x)
    (let-values ([(result next-state) (proc x state)])
      (set! state next-state)
      result)))
(define (accum proc acc ls)
  (map (stateful-proc acc proc) ls))
