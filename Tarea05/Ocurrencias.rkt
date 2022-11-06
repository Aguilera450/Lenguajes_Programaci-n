(define (ocurrencias list1 list2)
  (define (ocurrencia list1 x)
    (if (empty? list1)
        0
        (if(equal? x (first list1))
           (+ 1 (ocurrencia (rest list1) x))
           (ocurrencia (rest list1) x)))
  )
  
  (if (empty? list2)
      empty
      (cons
       (cons (first list2) (ocurrencia list1 (first list2)))
       (ocurrencias list1 (rest list2))))
)
