#lang racket
(require rackunit
         "main.rkt")
(require mzlib/etc)
(provide tea-eval)

;; tea-eval : SExp -> String
(define (tea-eval sexp)
  (get-output-value
   (auto-cleanup-process
    (lambda (p out in err)
      (write-string (string-append
                     (file->string (build-path
                                    ;; this file's source directory
                                    (this-expression-source-directory)
                                    "environment.js"))
                     "\nEnvironmentModule(this);\n\n"
                     (tea->js sexp))
                    in)
      (flush-output in)
      ; throw away 5 header lines
      ;; (read-line out)
      ;; (read-line out)
      ;; (read-line out)
      ;; (read-line out)
      ;; (read-line out)
      ; read the output
      (display (read-string 10000 err)))
    "/usr/bin/env"
    "nodejs-repl")))

(define (get-output-value string)
  (if (eof-object? string)
      (error 'tea-eval "no output found from js interpreter")
      string
      ;; (let ([prefix "(js|  )> "]
      ;;       [value "[^> ].*"])
      ;;   (regexp-match (string-append "(" prefix ")+" "(" value ")$")
      ;;                 string))
      ))

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
