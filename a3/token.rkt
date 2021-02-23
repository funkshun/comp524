#lang racket

(define (token type (data #f))
  (list type data))

(define (punctuation-token str)
  (token
   (case str
     [("(") 'OPAREN]
     [(")") 'CPAREN]
     [(",") 'COMMA]
     [(";") 'SEMICOLON]
     [(".") 'PERIOD]
     [("{") 'OBRACE]
     [{"}"} 'CBRACE])))

(define (int-token str)
  (token 'INT (string->number str)))

(define (float-token str)
  (token 'FLOAT (string->number str)))

(define (name-token str)
  (case str
    [("def" "fun" "if"
            "not" "and" "or") (key-token str)]
    [else (token 'NAME (string->symbol str))]))

(define (key-token str)
  (token
   (case str
     [("def") 'DEF]
     [("fun") 'FUN]
     [("if") 'IF]
     [("not") 'NOT]
     [("and") 'AND]
     [("or") 'OR])))

(define (string-token str)
  (token 'STRING (string-trim str "\"")))

(define re-lookup
  (list
    (list #px"^[\r\n\t ]+" #f)
    (list #px"^-?\\d+(?=[\r\n\t (){},;.]|$)" int-token)
    (list #px"^-?\\d+\\.\\d+(?=[\r\n\t (){},;.]|$)" float-token)
    (list #px"^[\\(\\)\\{\\},;\\.]" punctuation-token)
    (list #px"^\"[^\"]*\"(?=[\r\n\t (){},;.]|$)" string-token)
    (list #px"^[^0-9(){},;.\r\n\t \"/][^(){},;.\r\n\t \"]*(?=[\r\n\t (){},;.]|$)" name-token)
    (list #px"^//[^\n]*(\n|$)" #f)
    (list #px"^/\\*.*\\*/(?=[\r\n\t (){},;.]|$)" #f)))

(provide re-lookup)
(provide token)
