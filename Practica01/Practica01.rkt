#lang plai
;; Punto 1
;; filtra-lista: (listof any) procedure -> (listof any)
(define (filtra-lista lista predicado)
  (if(empty? lista)
     '()
     (if(predicado (first lista))
        (append (list (first lista)) (filtra-lista (rest lista) predicado))
        (filtra-lista (rest lista) predicado))))

;; Punto 5
;; palindromo?: string -> boolean
(define (palindromo? cadena)
  (if (equal? (string->list cadena) (reverse (string->list cadena)))
      #t
      #f))


;; Pruebas Unitarias ==========================================================
;; Punto 1
(define (prueba-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 2 2) number?) (list 1 2 2 2)))

;; Punto 5
(define (prueba1-palindromo?)
  (test (palindromo? "parangaricutirimicuaro") #f))

(define (prueba2-palindromo?)
  (test (palindromo? "girafarig") #t))

(prueba1-palindromo?)