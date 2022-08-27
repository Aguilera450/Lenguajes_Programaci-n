#lang plai
;; Punto 1
;; filtra-lista: (listof any) procedure → (listof any)
(define (filtra-lista lista predicado)
  (if(empty? lista)
     '()
     (if(predicado (first lista))
        (append (list (first lista)) (filtra-lista (rest lista) predicado))
        (filtra-lista (rest lista) predicado))))

;; Pruebas Unitarias ==========================================================
;; Punto 1
(define (prueba-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 2 2) number?) (list 1 2 2 2)))