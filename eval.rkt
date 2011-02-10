#lang racket
(require "main.rkt"
         racket/runtime-path)
(provide tea-eval
         tea-program-eval)

(define-runtime-path js-environment-file "environment.js")

;; tea-eval : SExp -> String
(define (tea-eval sexp)
  (send-js-to-rhino (tea->js sexp)))

;; tea-eval : [ListOf SExp] -> String
(define (tea-program-eval sexps)
  (send-js-to-rhino (tea-program->js-program sexps)))

(define (send-js-to-rhino js-string)
  (get-output-value
   (auto-cleanup-process
    (lambda (p out in err)
      (write-string (string-append
                     ;; include the environment
                     (file->string js-environment-file)
                     "\nEnvironmentModule(this);\n\n"
                     ;; include our js code
                     js-string)
                    in)
      (flush-output in)
      (close-output-port in)
      (read-line err)                   ; throw away header
      (read-line err))
    "/usr/bin/env"
    "rhino")))

(define (get-output-value string)
  (let ([prefix "(js|  )> "]
        [value "[^> ].*"])
    (last (regexp-match (string-append "(" prefix ")+" "(" value ")$")
                        string))))

(define-syntax auto-cleanup-process
  (syntax-rules ()
    [(_ procedure command args ...)
     (begin
       (let-values ([(a-subprocess stdout stdin stderr)
                     (subprocess #f #f #f command args ...)])
         (let ([value (procedure a-subprocess stdout stdin stderr)])
           (subprocess-kill a-subprocess #t)
           (close-input-port stdout)
           (close-input-port stderr)
           (close-output-port stdin)
           value)))]))
