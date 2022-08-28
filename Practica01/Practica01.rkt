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


;; Punto 9
;; rota: (listof any) -> (listof (listof any))
(define (rota lista)
  (if (empty? lista)
      empty
      (rotaAux lista (length lista))))

(define (rotaAux lista longitud)
  (if (zero? longitud)
      empty
      (cons lista (rotaAux (rotaIzq lista) (- longitud 1)))))

(define (rotaIzq lista)
  (append (rest lista) (list (first lista))))
;; Pruebas Unitarias ==========================================================
;; Punto 1
(define (prueba-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 2 2) number?) (list 1 2 2 2)))

;; Punto 5
(define (prueba1-palindromo?)
  (test (palindromo? "parangaricutirimicuaro") #f))

(define (prueba2-palindromo?)
  (test (palindromo? "girafarig") #t))

;; Punto 9
(define (prueba1-rota)
  (test (rota (list 1 2 3)) (list (list 1 2 3) (list 2 3 1) (list 3 1 2))))

(define (prueba2-rota)
  (test (rota (list "hola" #f 5)) (list (list "hola" #f 5) (list #f 5 "hola") (list 5 "hola" #f))))

(prueba2-rota)