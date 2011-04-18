(define (sieve n)
  (let ((number-line (build-list n (lambda (x) (add1 x)))))
    (loop 2 n number-line)))

(define (loop n m ls)
  (if (<= n m)
      (loop (add1 n) m (remove-factors-of n ls))
      ls))

(define (remove-factors-of n ls)
  (let ((initial (sqr n)))
    (foldr (lambda (number others)
             (if (and (>= number initial)
                      (divisble? number n))
                 others
                 (cons number others)))
           '()
           ls)))

(define (divisble? number divisor) (= (modulo number divisor) 0))

(sieve 120)
;; should produce
#|
'(1
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29
  31
  37
  41
  43
  47
  53
  59
  61
  67
  71
  73
  79
  83
  89
  97
  101
  103
  107
  109
  113)
|#