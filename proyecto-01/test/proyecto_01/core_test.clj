(ns proyecto-01.core-test
  (:require [clojure.test :refer :all]
  [proyecto-01.core :refer :all]))


(deftest leer-entrada-test
  (testing "Prueba de la funcion: leer-entrada"
    (is (= "(hola mundo)" (with-in-str "(hola\nmundo)" (leer-entrada))))
    (is (= "123" (with-in-str "123" (leer-entrada))))  
  )
)




(deftest verificar-parentesis-test
  (testing "Prueba de la funcion: verificar-parentesis"
  (is (= 1 (verificar-parentesis "(hola 'mundo")))
  (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
  (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
  (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
  (is (= 0 (verificar-parentesis "(hola '(mundo) )")))
  )
)

(deftest actualizar-amb-test
  (testing "Prueba de la funcion: actualizar-amb"
  (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
  (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
  (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
  (is (= '(b 7) (actualizar-amb () 'b 7)))
  )
)


(deftest buscar-test
  (testing "Prueba de la funcion: buscar"
  (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
  (is (= (generar-mensaje-error :unbound-variable 'f)  (buscar 'f '(a 1 b 2 c 3 d 4 e 5)) ))
  )
)

(deftest buscar-indice-test
  (testing "Prueba de la funcion: buscar-indice"
  (is (= (buscar-indice 'c '(a 1 b 2 c 3 d 4 e 5)) 5 ))
  )
)

(deftest error?-test
  (testing "Prueba de la funcion: error?"
  (is (= false (error? '(1 2 3))))
  (is (= true (error? (list (symbol ";WARNING:") 'mal 'hecho))))
  )
)


(deftest proteger-bool-en-str-test
  (testing "Prueba de la funcion: proteger-bool-en-str"
  (is (= "(or %f %t)" (proteger-bool-en-str "(or #f #t)")))
  (is (= "(and (or %f %t) %t)" (proteger-bool-en-str "(and (or #f #t) #t)")))  
  (is (= "" (proteger-bool-en-str "")))
  )
)

(deftest restaurar-bool-test
  (testing "Prueba de la funcion: proteger-bool-en-str"
  (is (= "(or %f %t)" (proteger-bool-en-str "(or #f #t)")))
  (is (= "(and (or %f %t) %t)" (proteger-bool-en-str "(and (or #f #t) #t)")))  
  (is (= "" (proteger-bool-en-str "")))
  )
)

(deftest fnc-append-test
  (testing "Prueba de la funcion: fnc-append"
  (is (= '(1 2 3 4 5 6 7) (fnc-append '( (1 2) (3) (4 5) (6 7)))))
  (is (= '(1 2 3 4) (fnc-append '( (1) (2) (3) (4)))))
  (is (= (generar-mensaje-error :wrong-type-arg 'append 3) (fnc-append '( (1 2) 3 (4 5) (6 7)))))  
  (is (= (generar-mensaje-error :wrong-type-arg 'append 'A) (fnc-append '( (1 2) A (4 5) (6 7)))))
  )
)

(deftest fnc-equal?-test
  (testing "Prueba de la funcion: fnc-equal?"
  (is (= (generar-mensaje-error :wrong-number-args-prim-proc 'equal?) (fnc-equal? ())))
  (is (= (generar-mensaje-error :wrong-number-args-prim-proc 'equal?) (fnc-equal? '(A))))  
  (is (= (symbol "#t") (fnc-equal? '(A A))))
  (is (= (symbol "#f") (fnc-equal? '(A a))))
  (is (= (generar-mensaje-error :wrong-number-args-prim-proc 'equal?) (fnc-equal? '(A A A))))   
  (is (= (symbol "#t") (fnc-equal? '((1 1) (1 1)))))
  (is (= (symbol "#f") (fnc-equal? '((1 1) (2 1)))))
  )
)

(deftest fnc-read-test
  (testing "Funcion fnc-read"
    (is (= "(hola mundo)" (str (with-in-str "(hola\nmundo)" (fnc-read ())))))
    (is (= (generar-mensaje-error :io-ports-not-implemented 'read) (fnc-read '(1))))
    (is (= (generar-mensaje-error :wrong-number-args-oper 'read) (fnc-read '(1 2))))
    (is (= (generar-mensaje-error :wrong-number-args-oper 'read) (fnc-read '(1 2 3))))
  )
)

(deftest fnc-sumar-test
  (testing "Prueba de la funcion: fnc-sumar"
  (is (= 0 (fnc-sumar ())))
  (is (= 3 (fnc-sumar '(3))))  
  (is (= 7 (fnc-sumar '(3 4))))
  (is (= 12 (fnc-sumar '(3 4 5))))
  (is (= 18 (fnc-sumar '(3 4 5 6))))   
  (is (= (generar-mensaje-error :wrong-type-arg1 '+ 'A) (fnc-sumar '(A 4 5 6))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'A) (fnc-sumar '(3 A 5 6))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'A) (fnc-sumar '(3 4 A 6))))
  )
)

(deftest fnc-restar-test
  (testing "Prueba de la funcion: fnc-restar"
  (is (= (generar-mensaje-error :wrong-number-args) (fnc-restar ())))
  (is (= -3 (fnc-restar '(3))))
  (is (= -1 (fnc-restar '(3 4))))  
  (is (= -6 (fnc-restar '(3 4 5))))
  (is (= -12 (fnc-restar '(3 4 5 6))))
  (is (= (generar-mensaje-error :wrong-type-arg1 '- 'A) (fnc-restar '(A 4 5 6))))   
  (is (= (generar-mensaje-error :wrong-type-arg2 '- 'A) (fnc-restar '(3 A 5 6))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '- 'A) (fnc-restar '(3 4 A 6))))
  )
)

(deftest fnc-menor-test
  (testing "Prueba de la funcion: fnc-menor"
  (is (= (symbol "#t") (fnc-menor ())))
  (is (= (symbol "#t") (fnc-menor '(1))))
  (is (= (symbol "#t") (fnc-menor '(1 2))))  
  (is (= (symbol "#t") (fnc-menor '(1 2 3))))
  (is (= (symbol "#t") (fnc-menor '(1 2 3 4))))
  (is (= (symbol "#f") (fnc-menor '(1 2 2 4))))   
  (is (= (symbol "#f") (fnc-menor '(1 2 1 4))))
  (is (= (generar-mensaje-error :wrong-type-arg1 '< 'A) (fnc-menor '(A 1 2 4))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '< 'A) (fnc-menor '(1 A 1 4))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '< 'A) (fnc-menor '(1 2 A 4))))
  )
)

(deftest fnc-mayor-test
  (testing "Prueba de la funcion: fnc-mayor"
  (is (= (symbol "#t") (fnc-mayor ())))
  (is (= (symbol "#t") (fnc-mayor '(1))))
  (is (= (symbol "#t") (fnc-mayor '(2 1))))  
  (is (= (symbol "#t") (fnc-mayor '(3 2 1))))
  (is (= (symbol "#t") (fnc-mayor '(4 3 2 1))))
  (is (= (symbol "#f") (fnc-mayor '(4 2 1 1))))   
  (is (= (symbol "#f") (fnc-mayor '(4 2 1 4))))
  (is (= (generar-mensaje-error :wrong-type-arg1 '> 'A) (fnc-mayor '(A 3 2 1))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '> 'A) (fnc-mayor '(3 A 2 1))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '> 'A) (fnc-mayor '(3 2 A 1))))
  )
)

(deftest fnc-mayor-o-igual-test
  (testing "Prueba de la funcion: fnc-mayor-o-igual"
  (is (= (symbol "#t") (fnc-mayor-o-igual ())))
  (is (= (symbol "#t") (fnc-mayor-o-igual '(1))))
  (is (= (symbol "#t") (fnc-mayor-o-igual '(2 1))))  
  (is (= (symbol "#t") (fnc-mayor-o-igual '(3 2 1))))
  (is (= (symbol "#t") (fnc-mayor-o-igual '(4 3 2 1))))
  (is (= (symbol "#t") (fnc-mayor-o-igual '(4 2 1 1))))   
  (is (= (symbol "#f") (fnc-mayor-o-igual '(4 2 1 4))))
  (is (= (generar-mensaje-error :wrong-type-arg1 '>= 'A) (fnc-mayor-o-igual '(A 3 2 1))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '>= 'A) (fnc-mayor-o-igual '(3 A 2 1))))
  (is (= (generar-mensaje-error :wrong-type-arg2 '>= 'A) (fnc-mayor-o-igual '(3 2 A 1))))
  )
)

(deftest evaluar-escalar-test
  (testing "Prueba de la funcion: evaluar-escalar"
  (is (= '(32 (x 6 y 11 z "hola")) (evaluar-escalar 32 '(x 6 y 11 z "hola"))))
  (is (= '("chau" (x 6 y 11 z "hola")) (evaluar-escalar "chau" '(x 6 y 11 z "hola"))))
  (is (= '(11 (x 6 y 11 z "hola")) (evaluar-escalar 'y '(x 6 y 11 z "hola"))))  
  (is (= '("hola" (x 6 y 11 z "hola")) (evaluar-escalar 'z '(x 6 y 11 z "hola"))))
  (is (= (list (generar-mensaje-error :unbound-variable 'n) '(x 6 y 11 z "hola")) (evaluar-escalar 'n '(x 6 y 11 z "hola"))))  
  )
)

(deftest evaluar-define-test
  (testing "Prueba de la funcion: evaluar-define"
  (is (= (list (symbol "#<void>") '(x 2)) (evaluar-define '(define x 2) '(x 1))))
  (is (= (list (symbol "#<void>") '(x 1 f (lambda (x) (+ x 1))))   (evaluar-define '(define (f x) (+ x 1)) '(x 1))) )
  (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define) ) '(x 1)) (evaluar-define '(define) '(x 1))) )
  (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define x) ) '(x 1)) (evaluar-define '(define x) '(x 1))) )
  (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define x 2 3) ) '(x 1)) (evaluar-define '(define x 2 3) '(x 1))) )
  (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define ()) ) '(x 1)) (evaluar-define '(define ()) '(x 1))) )
  (is (= (list (generar-mensaje-error :bad-variable 'define '(define () 2) ) '(x 1)) (evaluar-define '(define () 2) '(x 1))) )
  (is (= (list (generar-mensaje-error :bad-variable 'define '(define 2 x) ) '(x 1)) (evaluar-define '(define 2 x) '(x 1))) )
  )
)

(deftest evaluar-if-test
  (testing "Prueba de la funcion: evaluar-if"
  (is (= '(2 (n 7)) (evaluar-if '(if 1 2) '(n 7))))
  (is (= '(7 (n 7)) (evaluar-if '(if 1 n) '(n 7))) )
  (is (= '(7 (n 7)) (evaluar-if '(if 1 n 8) '(n 7))) )
  (is (= (list (symbol "#f") (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))) )
  (is (= (list 8 (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))) )
  (is (= (list (symbol "#<void>") (list 'n 9 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))) )
  (is (= (list (generar-mensaje-error :missing-or-extra 'if '(if) ) '(n 7)) (evaluar-if '(if) '(n 7))) )
  (is (= (list (generar-mensaje-error :missing-or-extra 'if '(if 1) ) '(n 7)) (evaluar-if '(if 1) '(n 7))) )
   )
)


(deftest evaluar-or-test
  (testing "Prueba de la funcion: evaluar-or"
  (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  (is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  (is (= (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  (is (= (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  )
)


(deftest evaluar-set!-test
  (testing "Prueba de la funcion: evaluar-set!"
  (is (= (list (symbol "#<void>") '(x 1))  (evaluar-set! '(set! x 1) '(x 0))))
  (is (= (list (symbol "#<void>") '(x 1 y "hola" z "chau" a 3))  (evaluar-set! '(set! x 1) '(x 0 y "hola" z "chau" a 3))))
  (is (= (list (generar-mensaje-error :unbound-variable 'x) '())   (evaluar-set! '(set! x 1) '())))
  (is (= (list (generar-mensaje-error :missing-or-extra 'set! '(set! x)) '(x 0))   (evaluar-set! '(set! x) '(x 0))))
  (is (= (list (generar-mensaje-error :missing-or-extra 'set! '(set! x 1 2)) '(x 0))   (evaluar-set! '(set! x 1 2) '(x 0))))
  (is (= (list (generar-mensaje-error :bad-variable 'set! 1) '(x 0)) (evaluar-set! '(set! 1 2) '(x 0))))
  )
)


; ---- TESTS FUNCIONES AUXILIARES ----

(deftest buscar-indice-test
  (testing "Prueba de la funcion: buscar-indice"
   (is (= 5 (buscar-indice 'c '(a 1 b 2 c 3))))
   (is (= nil (buscar-indice 'r '(a 1 b 2 c 3))))
  )
)

(deftest actualizar-amb-agregar-clave-valor-test
  (testing "Prueba de la funcion: actualizar-amb-agregar-clave-valor"
   (is (= '(a 1 b 2 c 3 d 6) (actualizar-amb-agregar-clave-valor '(a 1 b 2 c 3) 'd 6)))
   (is (= '(a 1 b 2) (actualizar-amb-agregar-clave-valor '(a 1) 'b 2)))
   (is (= '(a 1) (actualizar-amb-agregar-clave-valor () 'a 1)))
  )
)

(deftest buscar-aux-test
  (testing "Prueba de la funcion: buscar-aux"
   (is (= true (buscar-aux 'f '(f o r m a l e s))))
   (is (= false (buscar-aux 'd '(f o r m a l e s))))
   (is (= false (buscar-aux 'R '(f o r m a l e s))))
  )
)

(deftest esFalso?-test
  (testing "Prueba de la funcion: esFalso?"
   (is (= false (esFalso? 'd)))
   (is (= true (esFalso? (symbol "#f"))))
   (is (= true (esFalso? (symbol "#F"))))
   (is (= false (esFalso? (symbol "#t"))))
  )
)

(deftest pos-pares-test
  (testing "Prueba de la funcion: pos-pares"
   (is (= '(1 2 3) (pos-pares '(a 1 b 2 c 3))))
   (is (= '((symbol "#f") "hola" "chau") (pos-pares '(a (symbol "#f") b "hola" c "chau"))))
  )
)

(deftest pos-impares-test
  (testing "Prueba de la funcion: pos-impares"
   (is (= '(a b c) (pos-impares '(a 1 b 2 c 3))))
   (is (= '(a b c) (pos-impares '(a (symbol "#f") b "hola" c "chau"))))
  )
)

(deftest encontrar-elemento-no-numerico-test
  (testing "Prueba de la funcion: encontrar-elemento-no-numerico"
   (is (= '[0 R] (encontrar-elemento-no-numerico '(R 3 2))))
   (is (= '[3 A] (encontrar-elemento-no-numerico '(1 2 3 A 4))))
  )
)


(deftest or-dos-elementos-test
  (testing "Prueba de la funcion: or-dos-elementos"
   (is (= 1 (or-dos-elementos '1 '2)))
   (is (= 'b (or-dos-elementos 'b 'c)))
   (is (= (symbol "#f") (or-dos-elementos (symbol "#f") (symbol "#f"))))
   (is (= (symbol "#t") (or-dos-elementos (symbol "#f") (symbol "#t"))))
   (is (= (symbol "#t") (or-dos-elementos (symbol "#t") (symbol "#f"))))
  )
)

(deftest test-evaluar-if-aux
  (testing "Prueba de la funcion: evaluar-if-aux"
    (is (= '(2 (n 7)) (evaluar-if-aux 1 2 '(n 7))))
    (is (= '(7 (n 7)) (evaluar-if-aux (symbol "#f") 'n '(n 7))))
    (is (= '(7 (n 7)) (evaluar-if-aux (symbol "#f") 'n 8 '(n 7))))
    (is (= '(2 (n 7)) (evaluar-if-aux 1 2 (symbol "#f") '(n 7))))
    (is (= '(2 (n 7)) (evaluar-if-aux 1 2 '(n 7))))
  )
)

(deftest test-evaluar-or-aux
  (testing "Prueba de la funcion: evaluar-or-aux"
    (is (= (list (symbol "#t") '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or-aux '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")) (symbol "#t") '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list 5 '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or-aux '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")) 5 '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list 7 '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or-aux '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")) 7 '((symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  )
)
