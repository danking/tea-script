#lang racket
(require "main.rkt"
         racket/runtime-path)
(provide translate-file)

(define-runtime-path js-environment-file "environment.js")

(define (translate-file path)
  (display-to-file (string-append
                    ;; include the environment
                    (file->string js-environment-file)
                    "\nEnvironmentModule(this);\n\n"
                    ;; include our js code
                    (tea-program->js-program (file->sexps path)))
                   "a.js"
                   #:exists 'replace))

(define (file->sexps path)
  (call-with-input-file path
    (lambda (file-port)
      (let loop ((previous-datum (read file-port)))
        (if (not (eof-object? previous-datum))
            (cons previous-datum (loop (read file-port)))
            '())))))
