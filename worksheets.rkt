#lang racket

;; 27-01 Worksheet
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (indexer xs)
  (letrec ([indh (lambda (xs n)
                   (if (null? xs)
                       null
                       (cons (cons n (car xs))
                             (indh (cdr xs) (add1 n)))))])
    (indh xs 0)))
(indexer '(0 1 2 3 4 5))

;; 03/03

(define (update-binding env name val)
  (if (hash-has-key? env name)
      (hash-set env name val)
      (error "Key not found")))
