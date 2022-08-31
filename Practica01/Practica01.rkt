#lang plai
;; Punto 1
;; filtra-lista: (listof any) procedure -> (listof any)
(define (filtra-lista lista predicado)
  (if(empty? lista)
     '()
     (if(predicado (first lista))
        (append (list (first lista)) (filtra-lista (rest lista) predicado))
        (filtra-lista (rest lista) predicado))))

;; Punto 2
;; tipos-lista: (listof any) -> (listof string)
(define (tipos-lista lista)
    (if (empty? lista)
        empty
        (let* ([elem (first lista)]
                [res (cond
                      [(boolean? elem) "boolean"]
                      [(number? elem) "number"]
                      [(char? elem) "char"]
                      [(string? elem) "string"]
                      [(symbol? elem) "symbol"]
                      [(list? elem) "list"]
                      [(pair? elem) "pair"]
                      [else "Otra cosa xd"])])
            (append (list res) (tipos-lista (rest lista))))))

;; Punto 3
;; raro?: number -> boolean
(define (raro? num)
  (if (= (sumaDigitos num (length (string->list(number->string num)))) num)
      #t
      #f))

(define (sumaDigitos x long) 
  (if (= x 0)
      0
      (+ (expt (modulo x 10) long) (sumaDigitos (/ (- x (modulo x 10)) 10) long))))

;; Punto 4
;; descendente?: number* -> boolean
(define descendente? (lambda nums
    (apply >= nums)))

;; Punto 5
;; palindromo?: string -> boolean
(define (palindromo? cadena)
  (if (equal? (string->list cadena) (reverse (string->list cadena)))
      #t
      #f))

;; Punto 6
;; primo?: number → boolean
(define (primo? numero)
  (primoAux numero 2))

(define (primoAux num div)
  (cond
    [(and (<= num 2) (= num 2)) #t]
    [(and (<= num 2) (not (= num 2))) #f]
    [(= (modulo num div) 0) #f]
    [(> (* div div) num) #t]
    [else (primoAux num (+ div 1))]))

;; Punto 7
;; num-comb-monedas: number -> number


;; Punto 8
;; prom-mod-med: (listof number) -> number (listof number) number


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

;; Punto 10

;; Punto 11 Pruebas Unitarias ==========================================================
;; Punto 1
(define (prueba1-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 2 2) number?) (list 1 2 2 2)))

(define (prueba2-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 (list 1 2 3) 4) list?) (list (list 1 2 3))))

;; Punto 2
(define (prueba1-tipos-lista)
  (test (tipos-lista (list 1 "hola" 2 (list 1 2) 3 'a 'b)) (list "number" "string" "number" "list" "number" "symbol" "symbol")))

(define (prueba2-tipos-lista)
  (test (tipos-lista (list 1 2 "hola" #\a 3 4 empty "adios")) (list "number" "number" "string" "char" "number" "number" "list" "string")))

;; Punto 3
(define (prueba1-raro?)
  (test (raro? 12) #f))

(define (prueba2-raro?)
  (test (raro? 153) #t))

;; Punto 4
(define (prueba1-descendente?)
  (test (descendente? 5 4 6 2 1) #f))

(define (prueba2-descendente?)
  (test (descendente? 5 4 3 2 1) #t))

;; Punto 5
(define (prueba1-palindromo?)
  (test (palindromo? "Saken las kwamas") #f))

(define (prueba2-palindromo?)
  (test (palindromo? "girafarig") #t))

;; Punto 6
(define (prueba1-primo?)
  (test (primo? 8) #f))

(define (prueba2-primo?)
  (test (primo? 11) #t))

;; Punto 7

;; Punto 8


;; Punto 9
(define (prueba1-rota)
  (test (rota (list 1 2 3)) (list (list 1 2 3) (list 2 3 1) (list 3 1 2))))

(define (prueba2-rota)
  (test (rota (list "hola" #f 5)) (list (list "hola" #f 5) (list #f 5 "hola") (list 5 "hola" #f))))

;; punto 10

