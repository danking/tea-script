#lang racket
(require rackunit
         "main.rkt")
(provide tea-eval)

;; tea-eval : SExp -> (list String String)
;; the first string is (hopefully) the output
;; the second string is the full output of the command, check this if the first
;; doesn't make any sense
(define (tea-eval sexp)
  (let ([lines (read-all-lines (first (process
                                       (format
                                        (string-append "echo '~a' | "
                                                       "rhino 2>&1 | "
                                                       "sed 's/js> //'")
                                        (tea->js sexp)))))])
    (list (lines->output lines)
          (lines->string lines))))

(define (read-all-lines port)
  (let [(line (read-line port))]
   (cond [(eof-object? line) '()]
         [else (cons line (read-all-lines port))])))

(define (lines->output lines)
  (if (>= (length lines) 3)
      (third (reverse lines))
      (last lines)))

(define (lines->string lines)
  (foldl (lambda (line lines)
           (string-append lines "\n" line))
         ""
         lines))
