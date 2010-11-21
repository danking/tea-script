#lang racket
(require rackunit
         "../main.rkt")
(provide at-runtime-ts)

(define (run sexp)
  (read-all-lines (first (process (format (string-append "echo '~a' | "
                                                         "rhino 2>&1 | "
                                                         "sed 's/js> //'")
                                          (tea->js sexp))))))

(define (read-all-lines port)
  (let [(line (read-line port))]
   (cond [(eof-object? line) '()]
         [else (cons line (read-all-lines port))])))

(define (lines->output lines)
  (if (>= (length lines) 3)
      (third lines)
      (last lines)))

(define (lines->string lines)
  (foldl (lambda (line lines)
           (string-append lines "\n" line))
         ""
         lines))

(define-syntax check-runtime
  (syntax-rules ()
   [(_ sexp expected) (let [(output (run sexp))]
                        (check-equal? (lines->output (reverse output))
                                      expected
                                      (lines->string output)))]))

;; symbols
(define at-runtime-ts
 (test-suite
  "runtime tests"
  (test-case
   "non-js friendly ids"
   (check-runtime '(symbol=? foo-bar_ foo_bar-)
                  "false"))            ; depends on gensym
  (test-case
   "let"
   (check-runtime '(let ([x 3] [y 5])
                        (+ x y 2))
                  "10"))))

(require rackunit/text-ui)

(run-tests at-runtime-ts)