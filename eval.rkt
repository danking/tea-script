#lang racket
(require "main.rkt"
         racket/runtime-path)
(provide tea-eval)

(define-runtime-path js-environment-file "environment.js")

;; tea-eval : SExp -> String
(define (tea-eval sexp)
  (get-output-value
   (auto-cleanup-process
    (lambda (p out in err)
      (write-string (string-append
                     (file->string js-environment-file)
                     "\nEnvironmentModule(this);\n\n"
                     (tea->js sexp))
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
