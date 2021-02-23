#lang racket

(require "token.rkt")

(define (lex input)
  (reverse (step '() input)))

(define (step ast input)
  (if (zero? (string-length input))
      ast
      (let ([m (car
            (sort (matches input)
                  #:key sort-key
                  >))])
        (if (not (car m))
            (cons (token 'INVALID input) ast)
            (if (caadr m)
                (step (cons ((caadr m) (caar m)) ast)
                      (substring input (string-length (caar m))))
                (step ast
                      (substring input (string-length (caar m)))))))))

(define (matches str)
  (map (lambda (rx) (list (regexp-match (car rx) str) (cdr rx))) re-lookup))

(define (sort-key elem)
  (if (car elem)
      (string-length (caar elem))
      0))

(lex "\"This is a \"long test string.()\"")
