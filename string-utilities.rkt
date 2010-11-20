#lang racket
(provide ->csv string-repeat)

;; ->csv : [ListOf X] -> String
;; Same as ->csv* accept it returns the empty string when the list is empty
(define (->csv ->string ls)
  (if (empty? ls) "" (->csv* ->string ls)))

;; ->csv* : (cons Primitive [ListOf Primitive]) -> String
;;   where Primitive is (U String Symbol Number [ListOf Primitive])
;; produces a string in which the elements of the argument appear, seperated
;; by a comma and a space. No trailing commas. (e.g. '(1 2 3) => "[1, 2, 3]")
;; Does not accept empty lists.
(define (->csv* ->string ls)
  (if (empty? (rest ls))
      (->string (first ls))
      (string-append (->string (first ls))
                     ", "
                     (->csv* ->string (rest ls)))))


;; string-repeat : Natural String -> String
;; repeats the string n times and returns the appened result
(define (string-repeat n string)
  (if (> n 0)
      (string-append string
                     (string-repeat (- n 1)
                                    string))
      ""))