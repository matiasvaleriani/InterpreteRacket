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

## License

Copyright © 2023 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.

