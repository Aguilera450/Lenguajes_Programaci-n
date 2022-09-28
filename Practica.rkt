( define-type AST
              [ id ( i symbol? ) ]
[ num ( n number?) ]
[ bool ( b b o o l e a n ? ) ]
[ op ( f p r o c e d u r e ? ) ( a r g s ( l i s t o f AST ? ) ) ]
[ w i t h ( b i n d i n g s ( l i s t o f b i n d i n g ? ) ) ( body AST ? ) ]
[ w i t h ∗ ( b i n d i n g s ( l i s t o f b i n d i n g ? ) ) ( body AST ? ) ]
[ f u n ( pa rams ( l i s t o f symb ol ? ) ) ( body AST ? ) ]
[ app ( f u n AST? ) ( a r g s ( l i s t o f AST ? ) ) ]

)
( d e f i n e −t y p e B i n d i n g

[ b i n d i n g ( i d symb ol ? ) ( v a l u e AST ? ) ]

)


(define (parse sexp)
  (define (parse-op opsexp)
    (let(
         [operador (case (first opsexp)
                     [(+) +]
                     [(-) -]
                     [(*) *]
                     [(/) /]
                     [(modulo) modulo]
                     [(expt) expt]
                     [(not) not])])
      (op operador (map parse (rest opsexp)))
      )
    )
  (cond
    [(symbol? sexp)
     (case sexp
       [(T) (bool #t)]
       [(F) (bool #f)]
       [else (id sexp)]
       )]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+ - * / modulo expt not) (parse-op sexp)]
       )]
    )
  )

#|
    Ejercicio 2:
|#
(define (subst fwae-ast sub-id valor)
  (cond
    ; Si la expresión es un ID y puede ser el que estoy buscando
    [(id? fwae-ast) (if (= sub-id (id-i fwae-ast))
                        valor
                        fwae-ast
                        )]
    ; La expresión puede tener IDs y hay que buscar en ellos a sub-id
    [(op? fwae-ast) (op (op-f fwae-ast)
                        (map (lambda (arg) (subst arg sub-id valor)) (op-args fwae-ast)))]
    ; La expresión no puede tener IDs
    [else fwae-ast])
  )
