#lang racket
(require "main.rkt")
(provide translate-file)

(define (translate-file path)
  (display-to-file (tea-program->js-program (file->sexps path))
                   "a.out"
                   #:exists 'replace))

(define (file->sexps path)
  (call-with-input-file path
    (lambda (file-port)
      (let loop ((previous-datum (read file-port)))
        (if (not (eof-object? previous-datum))
            (cons previous-datum (loop (read file-port)))
            '())))))
