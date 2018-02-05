{-
Ejercicio #2 de La Tarea 1 sobre Haskell

Autores: Andrés Buelvas 13-10184
		    Miguel Canedo 13-10214

Fecha: 04/02/18
-}

{-
Descripción:
		Representación de un tipo Árbol Binario cons sus respectivas hojas y nodos.
-}
data ArbolBinario = L | N1 ArbolBinario | N ArbolBinario ArbolBinario deriving(Eq, Show)

crearArbolBinario :: Integer -> ArbolBinario
crearArbolBinario 1 = L
crearArbolBinario 2 = N1 L
crearArbolBinario n = N (crearArbolBinario (n `div` 2)) (crearArbolBinario ((n-1) `div` 2)) 

{-
Descripción:
		Función que calcula el tamaño de un Árbol Binario
-}
tam :: ArbolBinario -> Integer
tam L = 1
tam (N1 t1) = 1 + tam t1
tam (N t1 t2) = 1 + tam t1 + tam t2

{-
Descripción:
		Función que calcula la profundidad de un Árbol Binario.
-}
profundidad :: ArbolBinario -> Integer
profundidad L = 1
profundidad (N1 t1) = 1 + profundidad t1
profundidad (N t1 t2) = 1 + max (profundidad t1) (profundidad t2)

{-
Descripción:
		Función que crea un Árbol Binario.
-}
hacerABinTree :: Integer -> ArbolBinario
hacerABinTree 0 = error "No se puede crear un Árbol con 0 nodos"
hacerABinTree 1 = crearArbolBinario 1
hacerABinTree n = crearArbolBinario n


