# Interpete de Racket en Clojure
Trabajo Practico de **Lenguajes Formales (95.48)** de la **Facultad de Ingenieria de la Universidad de Buenos Aires** durante el **2C 2023**.
El objetivo del presente trabajo es construir un intérprete de Racket que corra en la JVM (Java Virtual Machine). Por ello, el lenguaje elegido para su implementación es **Clojure**.

## Ejecución desde Clojure

Se necesita cargar el archivo `/src/proyecto_01/core.clj`

```sh
user => (load-file "core.clj")
user => (repl)
(enter! "demo.rkt")
(enter! "jarras.rkt")
(breadth-first bc)
```

## Ejecución desde Leiningen

Correr interprete:
```sh
lein run
(enter! "demo.rkt")
(enter! "jarras.rkt")
(breadth-first bc)
```

 Correr pruebas unitarias: 
 ```sh
 lein test
 ```
