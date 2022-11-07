#lang plai

; Funcion recursiva que cuenta las ocurrencias de list2 en list1:
(define (ocurrencias list1 list2)
  (define (ocurrencia list1 x) ; Auxiliar que cuenta ocurrencias de x en list1
    (if (empty? list1)
        0
        (if(equal? x (first list1))
           (+ 1 (ocurrencia (rest list1) x))
           (ocurrencia (rest list1) x)))
  )
  
  (if (empty? list2)
      empty
      (cons (cons (first list2) (ocurrencia list1 (first list2)))
            (ocurrencias list1 (rest list2))))
)

; Funcion recursiva de cola que cuenta las ocurrencias de list2 en list1:
(define (cola_ocurrencias list1 list2)
  (ocurrencias_cola list1 list2 '())
)

; Funcion recursiva de cola que cuenta las apariciones de x en list:
(define (ocurrencia_cola list x acc)
  (if (empty? list)
      acc
      (if (equal? x (first list))
          (ocurrencia_cola (rest list) x (+ acc 1))
          (ocurrencia_cola (rest list) x acc)))
)

; Funcion recursiva de cola que implementa una optimizacion para encontrar las ocurrencias de list2 en list1:
(define (ocurrencias_cola list1 list2 acc)
  (if (empty? list2)
      acc
      (ocurrencias_cola       ;Argumentos de en la llamada recursiva:
       list1 (rest list2) 
       (cons acc (cons (first list2) (ocurrencia_cola list1 (first list2) 0))))))
