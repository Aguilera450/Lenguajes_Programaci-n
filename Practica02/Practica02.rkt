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
  [rombo (l number?) (D number?) (d number?)]
  [rectangulo (a number?) (b number?)]
  [elipse (a number?) (b number?)])

#|
            Ejercicio 2:
Definir las funciones area y perimetro.
|#
;; Definiendo π:
(define pi 3.141592653589793238)

;; perimetro: Figura →  number

(define (perimetro f)
  (cond
    [(paralelogramo? f)
     (+ (* 2 (paralelogramo-a f)) (* 2 (paralelogramo-b f)))]
    [(triangulo? f)
     (+ (triangulo-a f) (+ (triangulo-b f) (triangulo-c f)))]
    [(rectangulo? f)
     (+ (* 2 (rectangulo-a f)) (* 2 (rectangulo-b f)))]
    [(elipse? f)
     (let*([a (elipse-a f)]
           [b (elipse-b f)])
       (* pi (- (* 3 (+ a b))
                (sqrt (* (+ b (* 3 a))
                         (+ a (* 3 b)))))))]
    [(rombo? f)
     (* 4 (rombo-l f))]))

;; area: Figura →  number

(define (area f)
  (cond
    [(triangulo? f)
     (let*([s (/ (perimetro f) 2)]
           [a (triangulo-a f)]
           [b (triangulo-b f)]
           [c (triangulo-c f)])
       (sqrt (* s (* (- s a) (* (- s b) (- s c))))))]
    [(paralelogramo? f)
     (* (paralelogramo-a f) (paralelogramo-h f))]
    [(rectangulo? f)
     (* (rectangulo-a f) (rectangulo-b f))]
    [(elipse? f)
     (* pi (* (elipse-a f) (elipse-b f)))]
    [(rombo? f)
     (/ (* (rombo-D f) (rombo-d f)) 2)]))
