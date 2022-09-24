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

#|
                       Ejercicio 3:
El tipo de datos Vagon, que debe incluir 4 constructores;
uno por cada tipo de vagón descrito en el manual de prácticas:
|#
;; Tipo de dato Vagon y constructores

(define-type Vagon
  [dormitorio (camas positive-integer?)]
  [locomotora (p     positive-integer?)]
  [pasajeros  (cap   positive-integer?)]
  [restaurante
   (mesas    positive-integer?)
   (personal positive-integer?)])

;; Tipo de dato Tren

#|     Ejercicio 4
|#

;; Es el segundo punto de los ejercicios del 4.
(define-type Tren
  [tren-loc (vagon Vagon?)]
  [tren (loci locomotora?)
        (resto Tren?)
        (locd locomotora?)]
  [tren-t (vagon Vagon?)
        (resto Tren?)]
  [tren-f (resto Tren?)
        (vagon Vagon?)]
  )

(define (ac-potloc tren)
  (cond
    [(tren-loc? tren)(let (
         [vagon (tren-loc-vagon tren)])
         (if (locomotora? vagon)
             (locomotora-p vagon)
             0))]
    [(tren? tren)(let (
         [loci (tren-loci tren)]
         [locd (tren-locd tren)]
         [restot (tren-resto tren)])
         (+ (locomotora-p loci)
            (locomotora-p locd)
            (ac-potloc restot)))]
    [(tren-t? tren)(let (
         [vagon (tren-t-vagon tren)]
         [restot (tren-t-resto tren)])
         (+ (ac-potloc restot)
         (if (locomotora? vagon)
             (locomotora-p vagon)
             0)))]
    [(tren-f? tren)(let (
         [restot (tren-f-resto tren)]
         [vagon (tren-f-vagon tren)])
         (+ (ac-potloc restot)
            (if (locomotora? vagon)
                (locomotora-p vagon)
                0)))])
  )

(define (cuenta-vagones-noloc tren)
  (cond
    [(tren-loc? tren)(let (
         [vagon (tren-loc-vagon tren)])
         (if (locomotora? vagon)
             0
             1))]
         [(tren? tren)(let (
              [loci (tren-loci tren)]
              [locd (tren-locd tren)]
              [restot (tren-resto tren)])
              (cuenta-vagones-noloc restot))]
         [(tren-t? tren)(let (
              [vagon (tren-t-vagon tren)]
              [resto (tren-t-resto tren)])
              (+ (cuenta-vagones-noloc resto)
              (if (locomotora? vagon)
                  0
                  1)))]
         [(tren-f? tren)(let (
              [vagon (tren-f-vagon tren)]
              [restot (tren-f-resto tren)])
              (+ (cuenta-vagones-noloc restot)
              (if(locomotora? vagon)
                 0
                 1)))])
  )

(define (arrastre-usado tren)
  (/ (* (cuenta-vagones-noloc tren) 100 ) (ac-potloc tren))
 )

;; ==================================== Pruebas unitarias ====================================
;;                                        Perimetros:
;; Triángulo
(define (prueba-perimetro-triangulo1)
  (test (perimetro (triangulo 2 2 2)) 6))

(define (prueba-perimetro-triangulo2)
  (test (perimetro (triangulo 1 2 3)) 6))

;; Rectángulo
(define (prueba-perimetro-rectangulo1)
  (test (perimetro (rectangulo 2 2)) 8))

(define (prueba-perimetro-rectangulo2)
  (test (perimetro (rectangulo 1 3)) 8))

;; Rombo
(define (prueba-perimetro-rombo)
  (test (perimetro (rombo 3 4 2)) 12))

;; Paralelogramo
(define (prueba-perimetro-paralelogramo1)
  (test (perimetro (paralelogramo 2 2 2)) 8))

(define (prueba-perimetro-paralelogramo2)
  (test (perimetro (paralelogramo 4 2 2)) 12))

;; Elipse
(define (prueba-perimetro-elipse1)
  (test (perimetro (elipse 3 3)) 18.84955592153876))

(define (prueba-perimetro-elipse2)
  (test (perimetro (elipse 4 3)) 22.103491790916742))

;;                                          Áreas:
;; Triángulo
(define (prueba-area-triangulo1)
  (test (area (triangulo 2 2 2)) 1.7320508075688772))

(define (prueba-area-triangulo2)
  (test (area (triangulo 1 2 3)) 0))

;; Rectángulo
(define (prueba-area-rectangulo1)
  (test (area (rectangulo 2 2)) 4))

(define (prueba-area-rectangulo2)
  (test (area (rectangulo 1 4)) 4))

;; Rombo
(define (prueba-area-rombo)
  (test (area (rombo 3 4 2)) 4))

;; Paralelogramo
(define (prueba-area-paralelogramo1)
  (test (area (paralelogramo 2 2 2)) 4))

(define (prueba-area-paralelogramo2)
  (test (area (paralelogramo 4 2 2)) 8))

;; Elipse
(define (prueba-area-elipse1)
  (test (area (elipse 3 3)) 28.274333882308138))

(define (prueba-area-elipse2)
  (test (area (elipse 4 3)) 37.69911184307752))
