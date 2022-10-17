#lang plai

#|
Utilice los siguientes tipos de datos para representar los AST y las ataduras
(bindings) entre símbolos identificadores y sus valores FWAE:
|#
(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [with (bindings (listof binding?)) (body AST?)]
  [with* (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]
  [app (fun AST?) (args (listof AST?))])


(define-type Binding
  [binding (id symbol?) (value AST?)])

#|	Ejercicio 1
Defina la función (parse sexp), la cual recibe una expresión simbólica (symbolic
expression, s-expression); esto es, la expresión puede ser un número, un símbolo
o una lista de expresiones simbólicas. (parse sexp) debe construir un Árbol de
Sintaxis Abstracta (Abstract Syntax Tree - AST) a partir de la s-expression si
es una expresión válida en el lenguaje FWAE. En otro caso, debe arrojar un error.
|#
;; parse: s-expression -> AST
(define (parse sexp)
  (define (parse-op opsexp)
    (let([operador (case (first opsexp)
                     [(+) +]
                     [(-) -]
                     [(*) *]
                     [(/) /]
                     [(modulo) modulo]
                     [(expt) expt]
                     [(not) not])])
      (op operador (map parse (rest opsexp)))))
  (define (toBinding lista)
    (binding (first lista) (num (second lista))))
  (define (toLB lista)
    (if (empty? lista)
        '()
        (cons (toBinding (first lista)) (toLB(rest lista)))))
  (cond
    [(symbol? sexp)
     (case sexp
       [(T) (bool #t)]
       [(F) (bool #f)]
       [(+ - * / modulo expt not with with* fun app) (error "Esos simbolos ya existen padrino")]
       [else (id sexp)])]
    [(number? sexp) (num sexp)]
    [(list? sexp) (case (first sexp)
                    [(+ - * / modulo expt not) (parse-op sexp)]
                    [(with) (with (toLB (second sexp)) (parse (third sexp)))]
                    [(with*) (with* (toLB (second sexp)) (parse (third sexp)))]
                    [(fun) (fun (second sexp) (parse (third sexp)))]
                    [(app) (app (parse (second sexp)) (map parse (third sexp)))])]))





(parse '(app (fun (a b c d) 1) ((+ 1 1) b c)))
(parse '(+ 1 1))

#|	Ejercicio 2
Defina la función (subst fwae-expr sub-id value) que realiza la sustitución
fwae-expr[sub-id:=value]; es decir, reemplaza cada ocurrencia de la variable
sub-id en la expresión fwae-expr por otra expresión value. Cuide el alcance de
las variables conforme a las expresiones with, with* y app/fun (puesto que el
alcance es estático).
|#
;; subst: AST, symbol, AST -> AST

(define (subst fwae-ast sub-id valor)
  (cond
    [(id? fwae-ast) (if (symbol=? (id-i sub-id) (id-i fwae-ast))
                        valor
                        fwae-ast)]
    [(op? fwae-ast) (op (op-f fwae-ast)
                        (map (lambda (arg) (subst arg sub-id valor)) (op-args fwae-ast)))]
    [(with? fwae-ast) "With normalito"]
    [(with*? fwae-ast) "With feo"]
    [(fun? fwae-ast) (if (member (id-i sub-id) (fun-params fwae-ast))
                         1;;(rempla (id-i sub-id) valor (fun-body fwae-ast))
                         fwae-ast)]
    [(app? fwae-ast) (app (fun))]
    [else fwae-ast])
  )

;;(subst (id 'a) (id 'a) (num 1))
;;(subst (num 1) (id 'a) (num 4))
(subst (fun (list 'a 'b) (id 'a)) (id 'a) (num 1))
;;(subst (fun (list 'a 'b) (op + (list (id 'a) (num 1)))) (id 'a) (num 1))
;;(rempla 'a 1 '(a 'b 1))

;;(subst (with (list (binding 'a (num 1))) (num 1)) (id 'a) (num 1))
;;(subst (with (list (binding 'a (num 1)) (binding 'b (num 2))) (num 1) ) 'b (num 2))
;;(subst (op + (list (num 1) (num 1))) 'a 1)

;;(map binding? ('a (num 2)))
;;(alv '(1 2 3))
;;(AST? (with (list (binding 'a (num 1)) (binding 'b (num 2))) (num 1)))

#|	Ejercicio 3
Defina la función (interp fwae-expr) que evalúa la expresión FWAE dada.
Para evaluar expresiones with, with* y app es necesario utilizar la función subst.
|#
;; interp: AST -> number U boolean U AST-fun
(define (interp fwae-ast)
    (cond
        [(with? fwae-ast) (interp (foldl
			(lambda (bdg expr-res)
				(subst expr-res (binding-id bdg) (binding-value bdg))
			)
			(with-body fwae-ast)
            (with-bindings fwae-ast)))]
    )
)
