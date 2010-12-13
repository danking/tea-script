#lang racket
(require rackunit
         "main.rkt")
(provide tea-eval)

;; tea-eval : SExp -> (list String String)
;; the first string is (hopefully) the output
;; the second string is the full output of the command, check this if the first
;; doesn't make any sense
(define (tea-eval sexp)
  (let* ([rhino-process (process "rhino")]
         [rhino-stderr (fourth rhino-process)]
         [rhino-stdin (second rhino-process)])
    (write-string (string-append
                   ;; BROKEN: should refernece local file's
                   ;; directory
                   (file->string "environment.js")
                   "\nEnvironmentModule(this);\n\n"
                   (tea->js sexp))
                  rhino-stdin)
    (flush-output rhino-stdin)
    (read-line rhino-stderr) ; throw away the header
    (read-line rhino-stderr)))
