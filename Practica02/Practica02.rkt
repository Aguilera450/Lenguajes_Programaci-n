#lang plai
;;Integrantes del Equipo
;; Marco Antonio Rivera Silva 318183583
;; Adrian Aguilera Moreno 421005200
;; Kevin Jair Torres Valencia 318331818

;;Pregunta 1
;; Definimos el tipo de dato Figura
(define-type Figura
  [triangulo (a number?) (b number?) (c number?)]
  [rectangulo (a number?) (b number?)]
  [rombo (l number?) (D number?) (d number?)]
  [paralelogramo (a number?) (b number?) (h number?)]
  [elipse (a number?) (b number?)])


;;Pregunta 2
;; Perimetros
;; Funcion que obtiene el area de las figuras
(define (perimetro figura)
  (cond
    [(triangulo? figura) (perTr figura) ]
    [(rectangulo? figura) (perRec figura)]
    [(rombo? figura) (perRom figura)]
    [(paralelogramo? figura) (perPar figura)]
    [(elipse? figura) (perEli figura)]))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Triangulo
(define (perTr triangulo)
  (+ (triangulo-a triangulo) (triangulo-b triangulo) (triangulo-c triangulo)))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Rectangulo
(define (perRec rectangulo)
  (+ (* 2 (rectangulo-a rectangulo)) (* 2 (rectangulo-b rectangulo))))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Rombo
(define (perRom rombo)
  (* 4 (rombo-l rombo)))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Paralelogramo
(define (perPar para)
  (+ (* 2 (paralelogramo-a para)) (* 2 (paralelogramo-b para))))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro de la Elipse
(define (perEli eli)
  (* pi (- (* 3 (+ (elipse-a eli) (elipse-b eli))) (sqrt (* (+ (* 3 (elipse-a eli)) (elipse-b eli)) (+ (elipse-a eli) (* 3 (elipse-b eli))))))))

;; Areas

;; Funcion que obtiene el area de las figuras
(define (area figura)
  (cond
    [(triangulo? figura) (arTr figura) ]
    [(rectangulo? figura) (arRec figura)]
    [(rombo? figura) (arRom figura)]
    [(paralelogramo? figura) (arPar figura)]
    [(elipse? figura) (arEli figura)]))

;; Funcion auxiliar de area que sirve para calcular el area del Triangulo
(define (arTr tri)
  (let*([s (/ (+ (triangulo-a tri) (triangulo-b tri) (triangulo-c tri)) 2)])
    (sqrt (* s (- s (triangulo-a tri)) (- s (triangulo-b tri)) (- s (triangulo-c tri))))))

;; Funcion auxiliar de area que sirve para calcular el area del Rectangulo
(define (arRec rec)
  (* (rectangulo-a rec) (rectangulo-b rec)))

;; Funcion auxiliar de area que sirve para calcular el area del Rombo
(define (arRom rom )
  (/ (* (rombo-D rom) (rombo-d rom)) 2))

;; Funcion auxiliar de area que sirve para calcular el area del Paralelogramo
(define (arPar para)
  (* (paralelogramo-b para) (paralelogramo-h para)))

;; Funcion auxiliar de area que sirve para calcular el area de la elipse
(define (arEli eli)
  (* pi (elipse-a eli) (elipse-b eli)))

;; Pregunta 3
(define-type Vagon
  [locomotora (p positive-integer?)]
  [pasajeros (cap positive-integer?)]
  [restaurante (mesas positive-integer?) (personal positive-integer?)]
  [dormitorio (camas positive-integer?)])

;; Definimos un predicado para permitir una creacion de trenes similar a la de los ejemplos
;; (Perdon por el nombre xd)
(define (alv? a)
  (if (or (Tren? a) (Vagon? a))
      #t
      #f))

;; Definimos el tipo de dato Tren
(define-type Tren
  [tren-loc (a locomotora?)]
  [tren-f (a Tren?) (b alv?)])


;; Pregunta 4
;; Funcion que cuenta el numero de pasajeros del tren
(define (num-pasajeros tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (pasajeros? y)
                (+ (num-pasajeros x) (pasajeros-cap y))
                (+ (num-pasajeros x) 0))]))

;; Funcion que cuenta el arrastre usado por el tren
(define (arrastre-usado tren)
  (cond
    [(eq? (contar-vagones tren) 0) (* (contar-locomotoras tren) 100)]
    [else (* (/ (contar-vagones tren) (contar-locomotoras tren)) 100)]))

;; Funcion auxiliar de arrastre-tren que cuenta la potencia de la locomotoras del tren
(define (contar-locomotoras tren)
  (type-case Tren tren
    [tren-loc (x) (locomotora-p (tren-loc-a tren))]
    [tren-f (x y)
            (if (tren-loc? y)
                (+ (contar-locomotoras x) (locomotora-p (tren-loc-a y)))
                (+ (contar-locomotoras x) 0))]))

;; Funcion auxiliar de arrastre-tren que cuenta la cantidad de vagones del tren
(define (contar-vagones tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (Vagon? y)
                (+ (contar-vagones x) 1)
                (+ (contar-vagones x) 0))]))

;; Funcion que cuenta el numero de personas que se quedan si camita en el tren
(define (sin-cama tren)
  (if (>= (contar-camas tren) (contar-pasajeros tren))
      (- (contar-camas tren) (contar-pasajeros tren))
      (- (contar-pasajeros tren) (contar-camas tren))))

;; Funcion auxiliar de sin-cama que cuenta las camas del tren
(define (contar-camas tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (dormitorio? y)
                (+ (contar-camas x) (dormitorio-camas y))
                (+ (contar-camas x) 0))]))

;; Funcion auxiliar de sin-cama que cuenta el numero de pasajeros del tren
(define (contar-pasajeros tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (pasajeros? y)
                (+ (contar-pasajeros x) (pasajeros-cap y))
                (+ (contar-pasajeros x) 0))]))

;; Funcion que cuenta el numero de comensales maximos que se pueden atender en el tren
(define (max-comensales tren)
  (if (>= (* 8 (contar-personal tren)) (* 4 (contar-mesas tren)))
      (* 4 (contar-mesas tren))
      (* 8 (contar-personal tren))))

;; Funcion auxiliar de max-comensales que cuenta el personal del tren
(define (contar-personal tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (restaurante? y)
                (+ (contar-personal x) (restaurante-personal y))
                (+ (contar-personal x) 0))]))


;; Funcion auxiliar de max-comensales que cuenta las mesas del tren
(define (contar-mesas tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (restaurante? y)
                (+ (contar-mesas x) (restaurante-mesas y))
                (+ (contar-mesas x) 0))]))

;;Pruebas Unitarias

(define (prueba-perimetro-Triangulo)
  (test (perimetro (triangulo 3 4 5)) 12))

(define (prueba-area-Triangulo)
  (test (area (triangulo 3 4 5)) 6))

(define (prueba-perimetro-Rectangulo)
  (test (perimetro (rectangulo 5 6)) 22))

(define (prueba-area-Rectangulo)
  (test (area (rectangulo 5 6)) 30))

(define (prueba-perimetro-Rombo)
  (test (perimetro (rombo 3 6 8)) 12))

(define (prueba-area-Rombo)
  (test (area (rombo 3 6 8)) 24))

(define (prueba-perimetro-Paralelogramo)
  (test (perimetro (paralelogramo 3 6 8)) 18))

(define (prueba-area-Paralelogramo)
  (test (area (paralelogramo 3 6 8)) 48))

(define (prueba-perimetro-Elipse)
  (test (perimetro (elipse 3 3)) 18.84955592153876))

(define (prueba-area-Elipse)
  (test (area (elipse 3 3)) 28.274333882308138))

(define (prueba-num-pasajeros)
  (test (num-pasajeros (tren-f (tren-f (tren-loc (locomotora 4)) (pasajeros 15)) (pasajeros 16))) 31))

(define (prueba-arrastre-usado)
  (test (arrastre-usado (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (restaurante 5 2)) (dormitorio 10))) 300))

(define (prueba-sin-cama)
  (test (sin-cama (tren-f (tren-f (tren-loc (locomotora 1)) (dormitorio 20)) (pasajeros 40))) 20))
  
(define (prueba-max-comensales)
  (test (max-comensales (tren-f (tren-f (tren-loc (locomotora 1)) (restaurante 7 3)) (pasajeros 10))) 24))
