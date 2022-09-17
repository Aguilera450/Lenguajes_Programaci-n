#lang plai
;;Integrantes del Equipo
;; Marco Antonio Rivera Silva 318183583
;; Adrian Aguilera Moreno 421005200
;; Kevin Jair Torres Valencia 318331818

#| 
             Ejercicio 1:
Definir un tipo abstracto Figura que sea
utilizado para trabajar con geométricas,
deben tener los constructores indicados
|#

(define-type Figura
  [paralelogramo (a number?) (b number?) (h number?)]
  [triangulo (a number?) (b number?) (c number?)]
  [rombo (l number?) (d number?) (D number?)]
  [rectangulo (a number?) (b number?)]
  [elipse (a number?) (b number?)])

