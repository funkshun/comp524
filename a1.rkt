#lang racket

;; Question 1
(define (countdown n) 
  (if (< n 0)
      '()
      (cons n (countdown (- n 1)))))
(countdown 5)

;; Question 2
(define (remove-first s lst)
  (if (equal? '() lst) lst 
  (if (equal? s (car lst))
             (cdr lst)
             (cons (car lst) (remove-first s (cdr lst))))))
(remove-first 'y '(a b c))

(define (insert-after-every f s lst)
  (if (equal? '() lst) lst 
  (if (equal? f (car lst))
             (append (list f s) (insert-after-every f s (cdr lst)))
             (cons (car lst) (insert-after-every f s (cdr lst))))))
(insert-after-every 'x 'y '(x z z x y x))

;; Question 4
(define (zip xs ys) 
  (if (or (null? xs) (null? ys))
      '()
      (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))
(zip '(1 2 3 4 5 6) '(a b c))

;;Question 5
(define (append2 xs ys)
  (if (null? xs)
    ys
    (cons (car xs) (append2 (cdr xs) ys))))
(append2 '(4 20) '(1 2 3))

;; Question 6 No Reverse
(letrec ([binary->natural (lambda (num)
                            (if (null? num) 0
                            (if (equal? '1 (car num))
                                (+ (expt 2 (sub1 (length num))) (binary->natural (cdr num)))
                                (binary->natural (cdr num)))))])
  (binary->natural '(0 1 1)))

;; Question 6
(define (binary->natural n)
  (letrec ([binh (lambda (num p)
                   (if (null? num) 0
                       (if (equal? '1 (car num))
                           (+ (expt 2 p) (binh (cdr num) (add1 p)))
                           (binh (cdr num) (add1 p)))))])
    (binh n 0)))
(binary->natural '(0 0 1))

;; Question 7
(define (index-of-item s xs)
  (letrec ([ih (lambda (s xs pos)
                 (if (equal? s (car xs))
                     pos
                     (ih s (cdr xs) (add1 pos))))])
    (ih s xs 0)))
(index-of-item 'x '(y z x x))

;; Question 8
(define (divide n d)
  (letrec ([div (lambda (n d q)
                  (if (zero? n)
                      q
                      (div (- n d) d (add1 q))))])
    (div n d 0)))
(divide 25 5)

;; Question 9
(define (countdown2 n) (range n -1 -1))

;;Question 10
(define (mapzip xs ys)
  (map (lambda (x y)
         (list x y))
       xs
       ys))
(mapzip '(1 1 1 1) '(2 2 2 2))

;;Question 11
(define (sum-of-products xs ys)
  (letrec ([zip (map (lambda (x y)
                       (list x y))
                     xs
                     ys)]
           [prod (map (lambda (x)
                        (* (car x) (cadr x)))
                      zip)])
    (foldl + 0 prod)))

(sum-of-products '(1 3 5) '(2 4 6))

;; Question 12
(define (collatz-length n )
  (if (equal? 1 n)
      0
      (if (odd? n)
          (+ 1 (collatz-length (+ 1 (* 3 n))))
          (+ 1 (collatz-length (/ n 2))))))
(collatz-length 27)

;; Question 13
(define (catesian-product xs ys))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))
