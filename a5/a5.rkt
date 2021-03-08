#lang racket

(require (only-in (file "parse.rkt") parse))

(define (eval sc)
  (eval-program (parse sc)))

(define (eval-program program)
  (let* ([resList (eval-exprList (second program))])
    (last resList)))

(define (eval-exprList exprList)
  (let* ([fexpr (second exprList)]
         [optList (third exprList)])
    (cons (eval-expr fexpr) (eval-optExprList optList))))

(define (eval-optExprList optExprList)
    (if (< 1 (length optExprList))
        (eval-exprList (second optExprList))
        null))

(define (eval-expr expr)
  (let* ([child (second expr)]
         [child-label (first child)])
    (if (equal? child-label 'atom)
        (eval-atom child)
        (eval-invocation child))))

(define (eval-atom atom)
  (let* ([child (second atom)]
         [child-label (first child)]
         [child-val (second child)])
    (case child-label
      ['NAME (eval-name child)]
      ['STRING child-val]
      [else    (second child-val)])))

(define (eval-name name)
  (let* ([data (second name)])
    (proc-lookup data)))

(define (eval-invocation invoc)
  (let* ([argList (third invoc)]
         [args (eval-exprList argList)])
    (apply (first args) (rest args))))


(define (proc-lookup d)
  (case d
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['string-append string-append]
    ['string<? string<?]
    ['string=? string=?]
    ['not not]
    ['= =]
    ['< <]
    [else (error "Name not found")]))
