(ns proyecto-01.core
  (:gen-class))
(declare spy)
(declare repl)
(defn -main
  ;"Proyecto en Clojure"
  [& args]
  (repl))

(require '[clojure.string :as st :refer [blank? starts-with? ends-with? lower-case]]
         '[clojure.java.io :refer [delete-file reader]]
         '[clojure.walk :refer [postwalk postwalk-replace]])

(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x))
)

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-set!)
(declare evaluar-quote)
(declare evaluar-define)
(declare evaluar-enter!)
(declare evaluar-lambda)
(declare evaluar-escalar)


; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-car)
(declare fnc-cdr)
(declare fnc-env)
(declare fnc-not)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-list?)
(declare fnc-read)
(declare fnc-mayor)
(declare fnc-menor)
(declare fnc-null?)
(declare fnc-sumar)
(declare fnc-append)
(declare fnc-equal?)
(declare fnc-length)
(declare fnc-restar)
(declare fnc-display)
(declare fnc-newline)
(declare fnc-reverse)
(declare fnc-mayor-o-igual)
(declare fnc-caar)

; Funciones auxiliares

(declare buscar)
(declare error?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare leer-entrada)
(declare actualizar-amb)
(declare restaurar-bool)
(declare generar-nombre-arch)
(declare nombre-arch-valido?)
(declare controlar-aridad-fnc)
(declare proteger-bool-en-str)
(declare verificar-parentesis)
(declare generar-mensaje-error)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-de-cond)
(declare evaluar-secuencia-en-cond)

;Funciones auxiliares agregadas
(declare buscar-indice)
(declare eq?)
(declare actualizar-amb-agregar-clave-valor)
(declare buscar-aux)
(declare encontrar-elemento-no-numerico)
(declare evaluar-or-aux)
(declare esFalso?)
(declare or-dos-elementos)
(declare evaluar-if-aux)
(declare forma-lambda)
(declare pos-pares)
(declare pos-impares)

(defn repl
  "Inicia el REPL de Racket."
  ([]
   (println "Interprete de Racket en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2023") (prn) (flush)
   (repl (list 'append 'append 'car 'car 'cdr 'cdr 'cond 'cond 'cons 'cons 'define 'define
               'display 'display 'enter! 'enter! 'env 'env 'equal? 'equal? 'eval 'eval 'exit 'exit
               'if 'if 'lambda 'lambda 'length 'length 'list 'list 'list? 'list?
               'newline 'newline 'nil (symbol "#f") 'not 'not 'null? 'null? 'or 'or 'quote 'quote
               'read 'read 'reverse 'reverse 'set! 'set! (symbol "#f") (symbol "#f")
               (symbol "#t") (symbol "#t") '+ '+ '- '- '< '< '> '> '>= '>= 'caar 'caar) ""))
  ([amb ns]
   (if (empty? ns) (print ns) (pr ns)) (print "> ") (flush)
   (try
     (let [renglon (leer-entrada)]                       ; READ
          (if (= renglon "")
              (repl amb ns)
              (let [str-corregida (proteger-bool-en-str renglon),
                    cod-en-str (read-string str-corregida),
                    cod-corregido (restaurar-bool cod-en-str),
                    res (evaluar cod-corregido amb),     ; EVAL
                    res1 (first res),
                    res2 (second res)]                   
                    (cond 
                      (nil? res2) 'Goodbye!              ; Si el ambiente del resultado es 'nil', es porque se ha evaluado (exit)
                      (and (list? res1)                  ; En tal caso, sale del REPL devolviendo Goodbye!.
                           (= (first res1) 'ns))
                         (repl res2 (second res1))
                      :else
                         (do (imprimir res1)             ; PRINT
                             (repl res2 ns))))))         ; LOOP (Se llama a si misma con el nuevo ambiente)
     (catch Exception e                                  ; PRINT (si se lanza una excepcion)
                   (imprimir (generar-mensaje-error :error (get (Throwable->map e) :cause)))
                   (repl amb ns)))))                     ; LOOP (Se llama a si misma con el ambiente intacto)


(defn evaluar
  "Evalua una expresion `expre` en un ambiente. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb]
  ;(spy "Lo que llega a evaluar" expre)
  (if (and (seq? expre) (or (empty? expre) (error? expre))) ; si `expre` es () o error, devolverla intacta
      (list expre amb)                                     ; de lo contrario, evaluarla
      (cond
        (not (seq? expre))         (evaluar-escalar expre amb)
        (= (first expre) 'define)  (evaluar-define expre amb)
        (= (first expre) 'if)      (evaluar-if expre amb) 
        (= (first expre) 'or)      (evaluar-or expre amb)
        (= (first expre) 'set!)    (evaluar-set! expre amb)
        (= (first expre) 'cond)    (evaluar-cond expre amb)
        (= (first expre) 'eval)    (evaluar-eval expre amb)
        (= (first expre) 'exit)    (evaluar-exit expre amb)
        (= (first expre) 'lambda)  (evaluar-lambda expre amb)
        (= (first expre) 'enter!)  (evaluar-enter! expre amb)
        (= (first expre) 'quote)   (evaluar-quote expre amb)
        
	    	:else (let [res-eval-1 (evaluar (first expre) amb),
             						res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x))] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
					              (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2))))))


(defn aplicar
  "Aplica la funcion `fnc` a la lista de argumentos `lae` evaluados en el ambiente dado."
  ([fnc lae amb]
    ;(spy "lae" lae)
    ;(spy "fnc" fnc)
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb))
  ([resu1 resu2 fnc lae amb]
   (cond
     (error? resu1) (list resu1 amb)
     (error? resu2) (list resu2 amb)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb) amb)
     :else (aplicar-lambda fnc lae amb))))


(defn aplicar-lambda
  "Aplica la funcion lambda `fnc` a `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (not= (count lae) (count (second fnc))) (list (generar-mensaje-error :wrong-number-args fnc) amb)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb)
    :else (aplicar-lambda-multiple fnc lae amb)))


(defn aplicar-lambda-simple
  "Evalua un lambda `fnc` con un cuerpo simple"
  [fnc lae amb]
  (let [lae-con-quotes (map #(if (or (number? %) (string? %) (and (seq? %) (= (first %) 'lambda)))
                                 %
                                 (list 'quote %)) lae),
        nuevos-pares (reduce concat (map list (second fnc) lae-con-quotes)),
        mapa (into (hash-map) (vec (map vec (partition 2 nuevos-pares)))),
        cuerpo (first (nnext fnc)),
        expre (if (and (seq? cuerpo) (seq? (first cuerpo)) (= (ffirst cuerpo) 'lambda))
                  (cons (first cuerpo) (postwalk-replace mapa (rest cuerpo)))
                  (postwalk-replace mapa cuerpo))]
        (evaluar expre amb)))


(defn aplicar-lambda-multiple
  "Evalua una funcion lambda `fnc` cuyo cuerpo contiene varias partes."
  [fnc lae amb]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb))))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (= fnc '<)            (fnc-menor lae)
    (= fnc 'car)          (fnc-car lae)
    (= fnc 'cdr)          (fnc-cdr lae)
    (= fnc 'cons)         (fnc-cons lae)
    (= fnc 'display)      (fnc-display lae)
    (= fnc 'env)          (fnc-env lae amb)
    (= fnc 'length)       (fnc-length lae)
    (= fnc 'list)         (fnc-list lae)
    (= fnc 'list?)        (fnc-list? lae)
    (= fnc 'newline)      (fnc-newline lae)
    (= fnc 'not)          (fnc-not lae)
    (= fnc 'null?)        (fnc-null? lae)
    (= fnc 'reverse)      (fnc-reverse lae)
    (= fnc 'append)       (fnc-append lae)
    (= fnc 'equal?)       (fnc-equal? lae)
    (= fnc 'read)         (fnc-read lae)
    (= fnc '+)            (fnc-sumar lae)
    (= fnc '-)            (fnc-restar lae)
    (= fnc '>)            (fnc-mayor lae)
    (= fnc '>=)           (fnc-mayor-o-igual lae)
    (= fnc 'caar)         (fnc-caar lae)

    :else (generar-mensaje-error :wrong-type-apply fnc)))


(defn fnc-car
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'car), arg1 (first lae)]
       (cond
         (error? ari) ari
         (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'car arg1)
         :else (first arg1))))


(defn fnc-cdr
  "Devuelve una lista sin su 1ra. posicion."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'cdr), arg1 (first lae)]
       (cond
         (error? ari) ari
         (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'cdr arg1)
         :else (rest arg1))))


(defn fnc-cons
  "Devuelve el resultado de insertar un elemento en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 2 'cons), arg1 (first lae), arg2 (second lae)]
       (cond
         (error? ari) ari
					   	(not (seq? arg2)) (generar-mensaje-error :only-proper-lists-implemented 'cons)
					   	:else (cons arg1 arg2))))


(defn fnc-display
  "Imprime un elemento por la termina/consola y devuelve #<void>."
  [lae]
  (let [cant-args (count lae), arg1 (first lae)]
       (case cant-args
         1 (do (print arg1) (flush) (symbol "#<void>"))
         2 (generar-mensaje-error :io-ports-not-implemented 'display)
         (generar-mensaje-error :wrong-number-args-prim-proc 'display))))


(defn fnc-env
  "Devuelve el ambiente."
  [lae amb]
  (let [ari (controlar-aridad-fnc lae 0 'env)]
       (if (error? ari)
           ari
           amb)))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'length), arg1 (first lae)]
       (cond
         (error? ari) ari
         (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'length arg1)
         :else (count arg1))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1)
      ()
      lae))


(defn fnc-list?
  "Devuelve #t si un elemento es una lista. Si no, #f."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'list?), arg1 (first lae)]
       (if (error? ari)
           ari
           (if (seq? arg1)
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-newline
  "Imprime un salto de linea y devuelve #<void>."
  [lae]
  (let [cant-args (count lae)]
       (case cant-args
         0 (do (newline) (flush) (symbol "#<void>"))
         1 (generar-mensaje-error :io-ports-not-implemented 'newline)
         (generar-mensaje-error :wrong-number-args-prim-proc 'newline))))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'not)]
       (if (error? ari)
           ari
           (if (= (first lae) (symbol "#f"))
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-null?
  "Devuelve #t si un elemento es ()."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'null?)]
       (if (error? ari)
           ari
           (if (= (first lae) ())
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-reverse
  "Devuelve una lista con los elementos de `lae` en orden inverso."
  [lae]
    (let [ari (controlar-aridad-fnc lae 1 'reverse), arg1 (first lae)]
       (cond
         (error? ari) ari
         (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'reverse arg1)
         :else (reverse arg1))))


(defn controlar-aridad-fnc
  "Si la `lae` tiene la longitud esperada, se devuelve este valor (que es la aridad de la funcion).
   Si no, devuelve una lista con un mensaje de error."
  [lae val-esperado fnc]
  (if (= val-esperado (count lae))
      val-esperado
      (generar-mensaje-error :wrong-number-args-prim-proc fnc)))


(defn imprimir
  "Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas
  con comillas) y devuelve su valor. Muestra errores sin parentesis."
  ([elem]
   (cond
     (= \space elem) elem    ; Si es \space no lo imprime pero si lo devuelve
     (and (seq? elem) (starts-with? (apply str elem) ";")) (imprimir elem elem)
     :else (do (prn elem) (flush) elem)))
  ([lis orig]
   (cond
     (nil? lis) (do (prn) (flush) orig)
     :else (do (pr (first lis))
               (print " ")
               (imprimir (next lis) orig)))))


(defn revisar-fnc
  "Si la `lis` representa un error lo devuelve; si no, devuelve nil."
  [lis] (if (error? lis) lis nil))


(defn revisar-lae
  "Si la `lis` contiene alguna sublista que representa un error lo devuelve; si no, devuelve nil."
  [lis] (first (remove nil? (map revisar-fnc (filter seq? lis)))))


(defn evaluar-cond
  "Evalua una expresion `cond`."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
      (list (generar-mensaje-error :bad-or-missing 'cond expre) amb)
      (let [res (drop-while #(and (seq? %) (not (empty? %))) (next expre))]
            (if (empty? res) 
                (evaluar-clausulas-de-cond expre (next expre) amb)
                (list (generar-mensaje-error :bad-or-missing 'cond (first res)) amb)))))


(defn evaluar-clausulas-de-cond
  "Evalua las clausulas de cond."
  [expre lis amb]
  (if (nil? lis)
	     (list (symbol "#<void>") amb) ; cuando ninguna fue distinta de #f
		    (let [res-eval (if (not= (ffirst lis) 'else)
		                       (evaluar (ffirst lis) amb)
		                       (if (nil? (next lis))
		                           (list (symbol "#t") amb)
		                           (list (generar-mensaje-error :bad-else-clause 'cond expre) amb)))]
		         (cond
		           (error? (first res-eval)) res-eval
		           (= (first res-eval) (symbol "#f")) (recur expre (next lis) (second res-eval)) 
		           :else (evaluar-secuencia-en-cond (nfirst lis) (second res-eval))))))


(defn evaluar-secuencia-en-cond
  "Evalua secuencialmente las sublistas de `lis`. Devuelve el valor de la ultima evaluacion."
  [lis amb]
	  (if (nil? (next lis))
	      (evaluar (first lis) amb)
	      (let [res-eval (evaluar (first lis) amb)]
	           (if (error? (first res-eval))
   		           res-eval
  	             (recur (next lis) (second res-eval))))))


(defn evaluar-eval
  "Evalua una expresion `eval`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
      (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE <anon> ...")) amb)
      (let [arg (second expre)]
           (if (and (seq? arg) (= (first arg) 'quote))
               (evaluar (second arg) amb)
               (evaluar arg amb)))))


(defn evaluar-exit
  "Sale del interprete de Racket."
  [expre amb]
  (if (> (count expre) 2) ; si son el operador y mas de 1 argumento
      (list (generar-mensaje-error :wrong-number-args-prim-proc 'quit) amb)
      (list nil nil)))


(defn evaluar-lambda
  "Evalua una expresion `lambda`."
  [expre amb]
  (cond
    (< (count expre) 3) ; si son el operador solo o con 1 unico argumento
          (list (generar-mensaje-error :bad-body 'lambda (rest expre)) amb)
    (not (seq? (second expre)))
          (list (generar-mensaje-error :bad-params 'lambda expre) amb)
    :else (list expre amb)))


(defn evaluar-enter!
  "Carga en el ambiente un archivo 'expre' de código Racket"
  [expre amb]
  (cond
    (= (count expre) 1) ; si es el operador solo
      (list (generar-mensaje-error :wrong-number-args (symbol "enter!: bad syntax...")) amb)
    (= (second expre) (symbol "#f"))
      (list (list 'ns "") amb)
    :else (list (list 'ns (second expre)) (cargar-arch amb (second expre)))))

(defn cargar-arch
  "Carga el contenido de un archivo"
  ([amb arch]
   (let [res (evaluar arch amb),
         nom-original (first res),
         nuevo-amb (second res)]
         (if (error? nom-original)
             (do (imprimir nom-original) nuevo-amb)                 ; Mostrar el error
             (let [nom-a-usar (generar-nombre-arch nom-original)]
                   (if (error? nom-a-usar)
                       (do (imprimir nom-a-usar) nuevo-amb)          ; Mostrar el error
                       (let [tmp (try
                                    (slurp nom-a-usar)
                                    (catch java.io.FileNotFoundException _
                                      (generar-mensaje-error :file-not-found)))]
                            (if (error? tmp)
                                (do (imprimir tmp) nuevo-amb)        ; Mostrar el error
                                (do (spit "rkt-temp" (proteger-bool-en-str (clojure.string/replace tmp #"#lang racket" "")))
                                    (let [ret (with-open [in (java.io.PushbackReader. (reader "rkt-temp"))]
                                                (binding [*read-eval* false]
                                                  (try
                                                    (cargar-arch (second (evaluar (restaurar-bool (read in)) nuevo-amb)) in nom-original nom-a-usar)
                                                    (catch Exception e
                                                       (imprimir (generar-mensaje-error :end-of-file 'list))))))]
                                          (do (delete-file "rkt-temp" true) ret))))))))))
  ([amb in nom-orig nom-usado]
   (try
     (cargar-arch (second (evaluar (restaurar-bool (read in)) amb)) in nom-orig nom-usado)
     (catch Exception _
       amb))))


(defn generar-nombre-arch
  "Dada una entrada la convierte en un nombre de archivo de Racket valido."
  [nom]
  (if (not (string? nom))
      (generar-mensaje-error :wrong-type-arg1 'string-length nom)
      (let [n (lower-case nom)]
            (if (nombre-arch-valido? n)
                n
                (str n ".rkt")))))    ; Agrega '.rkt' al final


(defn nombre-arch-valido?
  "Chequea que el string sea un nombre de archivo .rkt valido."
  [nombre] (and (> (count nombre) 4) (ends-with? nombre ".rkt")))


(defn evaluar-quote
  "Evalua una expresion `quote`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
      (list (generar-mensaje-error :missing-or-extra 'quote expre) amb)
      (list (second expre) amb)))


(defn generar-mensaje-error
  "Devuelve un mensaje de error expresado como lista."
  ([cod]
 			(case cod 
         :file-not-found (list (symbol ";ERROR:") 'No 'such 'file 'or 'directory)
         :warning-paren (list (symbol ";WARNING:") 'unexpected (symbol "\")\"#<input-port 0>"))
         ()))
  ([cod fnc]
    (cons (symbol ";ERROR:")
    			(case cod
         :end-of-file (list (symbol (str fnc ":")) 'end 'of 'file)
         :error (list (symbol (str fnc)))
         :io-ports-not-implemented (list (symbol (str fnc ":")) 'Use 'of 'I/O 'ports 'not 'implemented)
         :only-proper-lists-implemented (list (symbol (str fnc ":")) 'Only 'proper 'lists 'are 'implemented)
         :unbound-variable (list 'unbound (symbol "variable:") fnc)
         :wrong-number-args (list 'Wrong 'number 'of 'args 'given fnc)
         :wrong-number-args-oper (list (symbol (str fnc ":")) 'Wrong 'number 'of 'args 'given)
         :wrong-number-args-prim-proc (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol (str fnc '>)))
         :wrong-type-apply (list 'Wrong 'type 'to 'apply fnc)
         ())))
  ([cod fnc nom-arg]
    (cons (symbol ";ERROR:") (cons (symbol (str fnc ":"))
    			(case cod
     			 :bad-body (list 'bad 'body nom-arg)
     			 :bad-else-clause (list 'bad 'ELSE 'clause nom-arg)
      			:bad-or-missing (list 'bad 'or 'missing 'clauses nom-arg)
     			 :bad-params (list 'Parameters 'are 'implemented 'only 'as 'lists nom-arg)
      			:bad-variable (list 'bad 'variable nom-arg)
     			 :missing-or-extra (list 'missing 'or 'extra 'expression nom-arg)
     			 :wrong-type-arg (list 'Wrong 'type 'in 'arg nom-arg)
     			 :wrong-type-arg1 (list 'Wrong 'type 'in 'arg1 nom-arg)
     			 :wrong-type-arg2 (list 'Wrong 'type 'in 'arg2 nom-arg)
         ())))))


; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE RACKET (ADEMAS DE COMPLETAR `EVALUAR` Y `APLICAR-FUNCION-PRIMITIVA`):

; user=> (leer-entrada)
; (hola
; mundo)
; "(hola mundo)"
; user=> (leer-entrada)
; 123
; "123"
; user=> (leer-entrada)
; (+ 1 3) 3)
; ;WARNING: unexpected ")"#<input-port 0>
; "(+ 1 3) 3)"
(defn leer-entrada
  "Lee una cadena desde la terminal/consola. Si contiene parentesis de menos al presionar Enter/Intro,
  se considera que la cadena ingresada es una subcadena y el ingreso continua. De lo contrario, se la
  devuelve completa (si corresponde, advirtiendo previamente que hay parentesis de mas)."
  []
  (flush)
  (loop [input ""
         parentesis-cont 0]
    (let [line (read-line)
          concat-input (if (empty? input)  line (str input " " line))
          new-parentesis-cont (verificar-parentesis concat-input)]
      (cond
        (pos? new-parentesis-cont) (recur concat-input new-parentesis-cont)
        (neg? new-parentesis-cont) (do
                                  (imprimir (generar-mensaje-error :warning-paren))
                                  concat-input
                                  )
        :else concat-input
      )
    )
  )
)


; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0
(defn verificar-parentesis
  "Cuenta los parentesis en una cadena, sumando 1 si `(`, restando 1 si `)`. Si el contador se hace negativo, para y retorna -1."
[cadena]
  (loop [contador 0
         chars (seq cadena)]
    (if (neg? contador)
      -1
      (if (empty? chars)
        contador
        (recur (condp = (first chars)
                 \( (inc contador)
                 \) (dec contador)
                 contador)
               (rest chars)))))
)

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
(defn actualizar-amb
  "Devuelve un ambiente actualizado con una clave y su valor. 
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza la nueva información."
  [amb clave valor]
  (if (not (error? valor))
    (if (buscar-aux clave amb)
      (let [posicion (buscar-indice clave amb)
            lista-resultante (if posicion
                               (-> amb
                                   (vec) 
                                   (assoc posicion valor) 
                                   vec) 
                               )]
        (apply list lista-resultante))
      (actualizar-amb-agregar-clave-valor amb clave valor)
    ) amb
  )
)

(defn actualizar-amb-agregar-clave-valor 
 "Agrega un par clave - valor en el ambiente"
 [amb clave valor]
  (let[amb-vector (-> amb (vec) (conj clave valor))]
  (apply list amb-vector)
  )
)


(defn buscar-aux
 "Devuelve true si la clave pertenece al ambiente, false en caso contrario"
 [clave ambiente]
  (let [clase-a-buscar (buscar-indice clave ambiente)]
    (if clase-a-buscar true false)))

; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)
(defn buscar [clave amb]
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   y devuelve el valor asociado. Devuelve un error :unbound-variable si no la encuentra."
	(let [lista-claves (pos-impares amb),
		lista-valores (pos-pares amb),
		index (.indexOf lista-claves clave)]
		(cond
			(= index -1) (generar-mensaje-error :unbound-variable clave)
		:else
			(nth lista-valores index)
		)
	)
)

(defn pos-pares [lista] 
	"Devuelve una lista con las posiciones pares de la lista recibida (valores del ambiente)"
	(map second (partition 2 lista))
)

(defn pos-impares [lista]
	"Devuelve una lista con las posiciones impares de la lista recibida (claves del ambiente)"
	(let [aux-pos-impares (map first (partition 2 lista)),
		len (count lista)]
		(cond
			(odd? len) (concat aux-pos-impares (list (last lista)))
		:else
			aux-pos-impares
		)
	)
)

(defn buscar-indice
  "Busca el índice de un elemento en una lista y devuelve la posición si se encuentra o nil si no se encuentra."
  [elem lista]
  (let [resultados (filter (comp not nil?) (map-indexed (partial eq? elem) lista))]
    (if (empty? resultados)
      nil
      (first resultados))))

(defn eq?
  "Compara un número con un índice y un elemento de la lista, y devuelve el índice + 1 si son iguales."
  [n i num]
  (if (= n num) (+ i 1) nil))


; user=> (error? (list (symbol ";ERROR:") 'mal 'hecho))
; true
; user=> (error? (list 'mal 'hecho))
; false
; user=> (error? (list (symbol ";WARNING:") 'mal 'hecho))
; true
(defn error?
  "Devuelve true o false, según sea o no el arg. una lista con `;ERROR:` o `;WARNING:` como primer elemento."
  [lista]
  (if (seq? lista)
    (cond
      (= (first lista) (symbol ";ERROR:")) true
      (= (first lista) (symbol ";WARNING:")) true
      :else false)
    false))


; user=> (proteger-bool-en-str "(or #f #t)")
; "(or %f %t)"
; user=> (proteger-bool-en-str "(and (or #f #t) #t)")
; "(and (or %f %t) %t)"
; user=> (proteger-bool-en-str "")
; ""
(defn proteger-bool-en-str
  "Cambia, en una cadena, #t por %t y #f por %f, para poder aplicarle read-string."
  [codigo]
  ;(spy "Entra en proteger-bool-en-str" codigo)
  ;(spy "De tipo" (type codigo))
  (let [codigo-final (-> codigo
                      (clojure.string/replace #"#f" "%f")
                      (clojure.string/replace #"#t" "%t")
                      (clojure.string/replace #"#F" "%F")
                      (clojure.string/replace #"#T" "%T"))]
  ;(spy "Sale de restaurar-bool de tipo: " (type codigo-final))
  
  codigo-final
  )
)


; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
(defn restaurar-bool
 "Cambia, en un codigo leido con read-string, %t por #t y %f por #f."
 [lista]
  (let [reemplazos {'%F (symbol "#F")
                    '%f (symbol "#f")
                    '%t (symbol "#t")
                    '%T (symbol "#T")}]

    (letfn [(procesar [elem]
              (if (sequential? elem)
                (map #(if (sequential? %) (procesar %) %) elem)
                (get reemplazos elem elem)))]

      (clojure.walk/prewalk-replace reemplazos lista))))



; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
(defn fnc-append 
"Devuelve el resultado de fusionar listas."  
  [listas]
  (reduce
    (fn [lista elem]
      (if (seq? elem)
        (concat lista elem)
        (reduced (generar-mensaje-error :wrong-type-arg 'append elem))))
    '()
    listas))

; user=> (fnc-equal? ())
; (;ERROR: Wrong number of args given #<primitive-procedure equal?>)
; user=> (fnc-equal? '(A))
; (;ERROR: Wrong number of args given #<primitive-procedure equal?>)
; user=> (fnc-equal? '(A A))
; #t
; user=> (fnc-equal? '(A a))
; #f
; user=> (fnc-equal? '(A A A))
; (;ERROR: Wrong number of args given #<primitive-procedure equal?>)
; user=> (fnc-equal? '((1 1) (1 1)))
; #t
; user=> (fnc-equal? '((1 1) (2 1)))
; #f
(defn fnc-equal?
  "Compara elementos. Si son iguales, devuelve true. Si no, false."
  [lista]
  (if (or (empty? lista) (not (even? (count lista)))) (generar-mensaje-error :wrong-number-args-prim-proc 'equal?)
    (if (apply = lista)
        (symbol "#t")
        (symbol "#f")
    ) 
  )
)



; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
(defn fnc-read [lista]
	 "Devuelve la lectura de un elemento de Racket desde la terminal/consola."
	(cond
		(= (count lista) 1) (generar-mensaje-error :io-ports-not-implemented 'read)
		(> (count lista) 1) (generar-mensaje-error :wrong-number-args-oper 'read)
	:else
		(restaurar-bool (read-string (proteger-bool-en-str (leer-entrada))))
	)
)


; user=> (fnc-sumar ())
; 0
; user=> (fnc-sumar '(3))
; 3
; user=> (fnc-sumar '(3 4))
; 7
; user=> (fnc-sumar '(3 4 5))
; 12
; user=> (fnc-sumar '(3 4 5 6))
; 18
; user=> (fnc-sumar '(A 4 5 6))
; (;ERROR: +: Wrong type in arg1 A)
; user=> (fnc-sumar '(3 A 5 6))
; (;ERROR: +: Wrong type in arg2 A)
; user=> (fnc-sumar '(3 4 A 6))
; (;ERROR: +: Wrong type in arg2 A)
(defn fnc-sumar
  "Suma los elementos de una lista."
  [lista]
  (let [elem-no-numerico (encontrar-elemento-no-numerico lista)
        elem-no-num (if (= (first elem-no-numerico ) 0) 1 2)
        nuevo-elem-no-numerico (assoc elem-no-numerico 0 elem-no-num)]
    (try
      (reduce + lista)
      (catch ClassCastException e
        (if (= 1 (first nuevo-elem-no-numerico)) (generar-mensaje-error :wrong-type-arg1 '+ (second nuevo-elem-no-numerico)) 
    (generar-mensaje-error :wrong-type-arg2 '+ (second nuevo-elem-no-numerico)))))))


(defn encontrar-elemento-no-numerico
  "Busca en una lista si hay un elemento no numerico, y devuelve el elemento y la posicion"
  [lista]
  (loop [index 0]
    (if (>= index (count lista))
      nil 
      (if (not (number? (nth lista index)))
        [index (nth lista index)] 
        (recur (inc index))))))


; user=> (fnc-restar ())
; (;ERROR: -: Wrong number of args given)
; user=> (fnc-restar '(3))
; -3
; user=> (fnc-restar '(3 4))
; -1
; user=> (fnc-restar '(3 4 5))
; -6
; user=> (fnc-restar '(3 4 5 6))
; -12
; user=> (fnc-restar '(A 4 5 6))
; (;ERROR: -: Wrong type in arg1 A)
; user=> (fnc-restar '(3 A 5 6))
; (;ERROR: -: Wrong type in arg2 A)
; user=> (fnc-restar '(3 4 A 6))
; (;ERROR: -: Wrong type in arg2 A)
(defn fnc-restar
  "Resta los elementos de una lista."
  [lista]
  ;Si el elemento no numerico esta en la posicion 0, me da arg1, sino arg2. Lo uso para los ERROR.
  (let [elem-no-numerico (encontrar-elemento-no-numerico lista)
        elem-no-num (if (= (first elem-no-numerico ) 0) 1 2)
        nuevo-elem-no-numerico (assoc elem-no-numerico 0 elem-no-num)]
    (if (empty? lista) (generar-mensaje-error :wrong-number-args)
    (try
      (if (= (count lista) 1) (- 0 (first lista)) (reduce - lista))
      (catch ClassCastException e
        (if (= 1 (first nuevo-elem-no-numerico)) (generar-mensaje-error :wrong-type-arg1 '- (second nuevo-elem-no-numerico)) 
    (generar-mensaje-error :wrong-type-arg2 '- (second nuevo-elem-no-numerico))))))
    ))

; user=> (fnc-menor ())
; #t
; user=> (fnc-menor '(1))
; #t
; user=> (fnc-menor '(1 2))
; #t
; user=> (fnc-menor '(1 2 3))
; #t
; user=> (fnc-menor '(1 2 3 4))
; #t
; user=> (fnc-menor '(1 2 2 4))
; #f
; user=> (fnc-menor '(1 2 1 4))
; #f
; user=> (fnc-menor '(A 1 2 4))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-menor '(1 A 1 4))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-menor '(1 2 A 4))
; (;ERROR: <: Wrong type in arg2 A)
(defn fnc-menor
  "Devuelve #t si los numeros de una lista estan en orden estrictamente creciente; si no, #f."
  [lista]
  (let [elem-no-numerico (encontrar-elemento-no-numerico lista)
        elem-no-num (if (= (first elem-no-numerico ) 0) 1 2)
        nuevo-elem-no-numerico (assoc elem-no-numerico 0 elem-no-num)]
    (if (every? number? lista) (if (empty? lista) (symbol "#t") (if (apply < lista) (symbol "#t") (symbol "#f")))
    (if (= 1 (first nuevo-elem-no-numerico)) (generar-mensaje-error :wrong-type-arg1 '< (second nuevo-elem-no-numerico)) 
    (generar-mensaje-error :wrong-type-arg2 '< (second nuevo-elem-no-numerico)))
    )
  )    
)

; user=> (fnc-mayor ())
; #t
; user=> (fnc-mayor '(1))
; #t
; user=> (fnc-mayor '(2 1))
; #t
; user=> (fnc-mayor '(3 2 1))
; #t
; user=> (fnc-mayor '(4 3 2 1))
; #t
; user=> (fnc-mayor '(4 2 2 1))
; #f
; user=> (fnc-mayor '(4 2 1 4))
; #f
; user=> (fnc-mayor '(A 3 2 1))
; (;ERROR: >: Wrong type in arg1 A)
; user=> (fnc-mayor '(3 A 2 1))
; (;ERROR: >: Wrong type in arg2 A)
; user=> (fnc-mayor '(3 2 A 1))
; (;ERROR: >: Wrong type in arg2 A)
(defn fnc-mayor
  "Devuelve #t si los numeros de una lista estan en orden estrictamente decreciente; si no, #f."
  [lista]
  (let [elem-no-numerico (encontrar-elemento-no-numerico lista)
        elem-no-num (if (= (first elem-no-numerico ) 0) 1 2)
        nuevo-elem-no-numerico (assoc elem-no-numerico 0 elem-no-num)]
    (if (every? number? lista) (if (empty? lista) (symbol "#t") (if (apply > lista) (symbol "#t") (symbol "#f")))
    (if (= 1 (first nuevo-elem-no-numerico)) (generar-mensaje-error :wrong-type-arg1 '> (second nuevo-elem-no-numerico)) 
    (generar-mensaje-error :wrong-type-arg2 '> (second nuevo-elem-no-numerico)))
    )
  )    
)


; user=> (fnc-mayor-o-igual ())
; #t
; user=> (fnc-mayor-o-igual '(1))
; #t
; user=> (fnc-mayor-o-igual '(2 1))
; #t
; user=> (fnc-mayor-o-igual '(3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 1 4))
; #f
; user=> (fnc-mayor-o-igual '(A 3 2 1))
; (;ERROR: >=: Wrong type in arg1 A)
; user=> (fnc-mayor-o-igual '(3 A 2 1))
; (;ERROR: >=: Wrong type in arg2 A)
; user=> (fnc-mayor-o-igual '(3 2 A 1))
; (;ERROR: >=: Wrong type in arg2 A)
(defn fnc-mayor-o-igual
  "Devuelve #t si los numeros de una lista estan en orden decreciente; si no, #f."
  [lista]
  (let [elem-no-numerico (encontrar-elemento-no-numerico lista)
        elem-no-num (if (= (first elem-no-numerico ) 0) 1 2)
        nuevo-elem-no-numerico (assoc elem-no-numerico 0 elem-no-num)]
    (if (every? number? lista) (if (empty? lista) (symbol "#t") (if (apply >= lista) (symbol "#t") (symbol "#f")))
    (if (= 1 (first nuevo-elem-no-numerico)) (generar-mensaje-error :wrong-type-arg1 '>= (second nuevo-elem-no-numerico)) 
    (generar-mensaje-error :wrong-type-arg2 '>= (second nuevo-elem-no-numerico)))
    )
  )    
)

; user=> (evaluar-escalar 32 '(x 6 y 11 z "hola"))
; (32 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar "chau" '(x 6 y 11 z "hola"))
; ("chau" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'y '(x 6 y 11 z "hola"))
; (11 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'z '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'n '(x 6 y 11 z "hola"))
; ((;ERROR: unbound variable: n) (x 6 y 11 z "hola"))
(defn evaluar-escalar [expre amb]
	"Evalua una expresion escalar. Devuelve una lista con el resultado y un ambiente."
	(cond
		(symbol? expre) (list (buscar expre amb) amb)
	:else
		(list expre amb)
	)
)


; user=> (evaluar-define '(define x 2) '(x 1))
; (#<void> (x 2))
; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<void> (x 1 f (lambda (x) (+ x 1))))
; user=> (evaluar-define '(define) '(x 1))
; ((;ERROR: define: missing or extra expression (define)) (x 1))
; user=> (evaluar-define '(define x) '(x 1))
; ((;ERROR: define: missing or extra expression (define x)) (x 1))
; user=> (evaluar-define '(define x 2 3) '(x 1))
; ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))
; user=> (evaluar-define '(define ()) '(x 1))
; ((;ERROR: define: missing or extra expression (define ())) (x 1))
; user=> (evaluar-define '(define () 2) '(x 1))
; ((;ERROR: define: bad variable (define () 2)) (x 1))
; user=> (evaluar-define '(define 2 x) '(x 1))
; ((;ERROR: define: bad variable (define 2 x)) (x 1))
(defn evaluar-define 
	"Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
  [expre amb]
  ;(spy "la expresion que entra" expre)
	(let [clave (second expre),
		len-expre (count expre)]
		(cond

			(< len-expre 3) (list (generar-mensaje-error :missing-or-extra 'define expre) amb)	; expresion con argumentos menor a 3 es error missing
			(and (> len-expre 3) (every? number? (drop 2 expre))) (list (generar-mensaje-error :missing-or-extra 'define expre) amb)

			(or
				(and (coll? clave) (= (count clave) 0)) ; si el segundo es una lista y tiene longitud 0
				(and (not (coll? clave)) (not (symbol? clave)))) ; si el segundo es un numero
					(list (generar-mensaje-error :bad-variable 'define expre) amb) ; es error bad variable

			(symbol? clave) (list (symbol "#<void>") (actualizar-amb amb clave (first (evaluar (nth expre 2) amb)))) ; si el segundo elemento es un symbol, se guarda lo que sigue en el ambiente como valor
		:else
			(let [clave (first (second expre)),
				func (forma-lambda expre)]
				(list (symbol "#<void>") (actualizar-amb amb clave func)) ; lambda
			)
		)
	)
)


(defn forma-lambda
  "Expresion lambda para definir funciones"
	([expre]
		(let [cabeza (drop 1 (second expre)),
			cuerpo (drop 2 expre)]
			(cons (symbol "lambda") (cons cabeza cuerpo))
		)
	)
)


; user=> (evaluar-if '(if 1 2) '(n 7)) 
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<void> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<void> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
(defn evaluar-if 
  "Evalua una expresion `if`. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
    [expre amb]
	(cond
		(= (count expre) 3) (evaluar-if-aux (nth expre 1) (nth expre 2) amb)
		(= (count expre) 4) (evaluar-if-aux (nth expre 1) (nth expre 2) (nth expre 3) amb)
	:else
		(list (generar-mensaje-error :missing-or-extra 'if expre) amb)
	)
)

(defn esFalso? 
 "Devuelve falso si la expresion es igual a #f o #F; true en caso contrario"
 [expre]
	(cond
		(= expre (symbol "#f")) true
		(= expre (symbol "#F")) true
	:else
		false
	)
)

(defn evaluar-if-aux
	([condicion valor-true amb]
		(evaluar-if-aux condicion valor-true (symbol "#f") amb)
	)

	([condicion valor-true valor-false amb]

		(let [evaluacion (evaluar condicion amb),
			eval-condicion (nth evaluacion 0),
			nuevo-amb (nth evaluacion 1)]

			(cond
				(not (esFalso? eval-condicion)) (evaluar valor-true nuevo-amb)
			:else
				(cond
					(= valor-false (symbol "#f")) (list valor-false nuevo-amb)
				:else
					(evaluar valor-false nuevo-amb)
				)
			)
		)
	)
)


; user=> (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#t (#f #f #t #t))
; user=> (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (7 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (5 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
(defn evaluar-or [expre amb]
	"Evalua una expresion `or`. Devuelve el primer valor distinto de `false` o `(symbol '#f')`."
	(cond
		(= (count expre) 1) (list (symbol "#f") amb)
	:else
		(evaluar-or-aux (drop 1 expre) amb)
	)
)

(defn or-dos-elementos [valor1 valor2]
	"Devuelve el elemento verdadero con prioridad en el primero, en caso de ser ambos falsos devuelve #f"
	(cond
		(not (esFalso? valor1)) valor1
		(not (esFalso? valor2)) valor2
	:else
		(symbol "#f")
	)
)


(defn evaluar-or-aux
	([lista amb]
		(let [evaluacion (evaluar (first lista) amb),
			primer-elemento (nth evaluacion 0),
			nuevo-amb (nth evaluacion 1)]
			(evaluar-or-aux (drop 1 lista) primer-elemento nuevo-amb) ; paso el primer elemento ya evaluado
		)
	)

	([lista resultado amb]
		(cond
			(empty? lista) (list resultado amb) ; se devuelve resultado ya evaluado
			(not (esFalso? resultado)) (list resultado amb) ; ya si el resultado no es falso, no hace falta evaluar los demas
		:else
			(let [evaluacion (evaluar (first lista) amb),
				primer-elemento (nth evaluacion 0),
				nuevo-amb (nth evaluacion 1),
				nuevo-resultado (or-dos-elementos resultado primer-elemento)] ; or-dos-elementos devolvera el verdadero de ambos (con prioridad en 'resultado')

				(evaluar-or-aux (drop 1 lista) nuevo-resultado nuevo-amb) ; llamo recursivamente sacando el primer-elemento y con el nuevo resultado (que pudo haberse mantenido igual)
			)
		)
	)
)

; user=> (evaluar-set! '(set! x 1) '(x 0))
; (#<void> (x 1))
; user=> (evaluar-set! '(set! x 1) '())
; ((;ERROR: unbound variable: x) ())
; user=> (evaluar-set! '(set! x) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x)) (x 0))
; user=> (evaluar-set! '(set! x 1 2) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))
; user=> (evaluar-set! '(set! 1 2) '(x 0))
; ((;ERROR: set!: bad variable 1) (x 0))

;AGREGO TEST
; user=> (evaluar-set! '(set! x 1) '(x 0 y "hola" z "chau" a 3) )
; (#<void> (x 1 y "hola" z "chau" a 3))
(defn evaluar-set! 
  "Evalua una expresion `set!`. Devuelve una lista con el resultado y un ambiente actualizado con la redefinicion."
  [expre amb]
	(cond
		(not (symbol? (second expre))) (list (generar-mensaje-error :bad-variable 'set! (second expre)) amb)
		(not= (count expre) 3) (list (generar-mensaje-error :missing-or-extra 'set! expre) amb)
	:else
		(let [evaluacion (evaluar (nth expre 2) amb)
			valor-guardar (nth evaluacion 0)
			nuevo-amb (nth evaluacion 1)
			clave (nth expre 1)
			valor-encontrado (buscar clave nuevo-amb)]

			(cond
				(error? valor-encontrado) (list valor-encontrado nuevo-amb)
			:else
				(list (symbol "#<void>") (actualizar-amb nuevo-amb clave valor-guardar))
			)
		)
	)
)

; ##### FUNCION AGREGADA PARA EL FINAL #####
; > (caar '( (a b c) d (e) ) )
; 'a
; > (caar 1 2 3)  
; ;ERROR: Wrong number of args given #<primitive-procedure caar>
; > (caar 1)
; ;ERROR: caar: Wrong type in arg 1
(defn fnc-caar
  "Devuelve el primer elemento del elemento de la primera posicion (deber ser una sublista)."
  [lista]
  (if (not= (count lista) 1) (generar-mensaje-error :wrong-number-args-prim-proc 'caar)
  (if (not (seq? (first lista))) (generar-mensaje-error :wrong-type-arg 'caar (first lista)) 
  (first (first (first lista))))  
  )
)

; Al terminar de cargar el archivo en el REPL de Clojure, se debe devolver true.
true