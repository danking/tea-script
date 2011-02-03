#lang racket
(require rackunit)

(define-syntax (require/run-test-suites stx)
  (define (test-suite-names->filenames test-suite-names)
    (map (lambda (test-suite-name)
           (string-append (symbol->string test-suite-name)
                          ".rkt"))
         test-suite-names))
  (define (test-suite-names->test-suite-ids test-suite-names)
    (map (lambda (test-suite-name)
           (string->symbol (string-append (symbol->string test-suite-name)
                                          "-ts")))
         test-suite-names))

  (syntax-case stx ()
    [(require/run-test-suites test-suite-name ...)
     (with-syntax
         ([(filename ...)
           (datum->syntax #'(test-suite-name ...)
                          (test-suite-names->filenames
                           (syntax->datum #'(test-suite-name ...))))]
          [(test-suite-id ...)
           (datum->syntax #'(test-suite-name ...)
                          (test-suite-names->test-suite-ids
                           (syntax->datum #'(test-suite-name ...))))])
      #'(begin
          (require rackunit/text-ui
                   filename ...)
          (run-tests (test-suite
                      "meta-test-suite"
                      test-suite-id ...))))]))

(require/run-test-suites parser
                         tea-to-js
                         sanitize-ids
                         js-expansions
                         js-to-text
                         integration
                         eval
                         string-utilities)
(display (port->string (first (process "rhino environment.js"))))
