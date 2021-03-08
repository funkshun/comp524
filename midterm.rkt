#lang racket

;; Q4
(define (list-product lst)
  (letrec ([prodh (lambda (xs)
                    (if (null? xs)
                        1
                        (* (car xs) (prodh (cdr xs)))))])
    (prodh lst)))

(define re-table
  (list
    (list #px"^[\r\n\t ]+" skip-match)
    (list #px"^\\d+(?=[\r\n\t ]|$)" int-t)
    (list #px"^[;]"semi-t)
    (list #px"[+\\-\\*/=](?=[\r\n\t ]|$)" op-t)
    (list #px"[a-z]+(?=[\r\n\t ]|$)" name-t)))

(define result
  '(disjunct
    (conjunct
     (conjunct
      (conjunct
      (atom
       (TRUE #f)))
      (AND #f)
      (atom
       (TRUE #f)))
     (AND #f)
     (atom
      (FALSE #f)))))

;; Q10
(define (binding-lvalue-pending?)
  (or (check 'NAME)
      (seq-names-pending?)
      (map-names-pending?)))

;; Q11
(define (parse-binding-lvalue)
  (if (seq-names-pending?)
      (list 'binding-lvalue (parse-seq-names))
      (if (map-names-pending?)
          (list 'binding-lvalue (parse-map-names))
          (list 'binding-lvalue (consume 'NAME)))))

(define (eval-arg-list arg-list)
  (list (eval-arg (second arg-list))))

(define (eval-arg-list-tail arg-list-tail)
  (if (equal? (length arg-list-tail) 1)
      '()
      (eval-arg-list (third arg-list-tail))))

(define (eval-let2 let-expr env)
  (let* ([1name-token (third let-expr)]
         [1name (second 1name-token)]
         [1val-expr (fourth let-expr)]
         [2name (second (sixth let-expr)) optExpr-expr]
         [2val-expr (seventh let-expr)]
         [body-expr (ninth let-expr)])
    (eval-expr body-expr
               (add-binding (add-binding 2name
                                         (eval-expr 2val-expr env))
                            1name
                            (eval-expr 1val-expr env)))))
