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
data ArbolBinario = L | N1 ArbolBinario | N2 ArbolBinario ArbolBinario deriving(Eq, Show)

{-
Descripción:
		Función que crea un Arbol Binario completo de 'n' niveles.
-}
crearArbolBinario :: Integer -> ArbolBinario
crearArbolBinario 0 = L
crearArbolBinario n = N2 (crearArbolBinario (n-1)) (crearArbolBinario (n-1))

{-
Descripción:
		Función que calcula el tamaño de un Árbol Binario, es decir,
		la cantidad de nodos que posee.
-}
tam :: ArbolBinario -> Integer
tam L = 1
tam (N1 t1) = 1 + tam t1
tam (N2 t1 t2) = 1 + tam t1 + tam t2

{-
Descripción:
		Función que calcula la profundidad de un Árbol Binario.
-}
profundidad :: ArbolBinario -> Integer
profundidad L = 1
profundidad (N1 t1) = 1 + profundidad t1
profundidad (N2 t1 t2) = 1 + max (profundidad t1) (profundidad t2)

{-
Descripción:
		Función que crea un Árbol Binario de exactamente 'n' nodos.
-}
hacerABinTree :: Integer -> ArbolBinario
hacerABinTree 1 = L
hacerABinTree 2 = (N1 L)
hacerABinTree n
	| n <= 0 = error "Error. No se puede crear un Árbol con esa cantidad de nodos."
	| n >= 3 = N2 (hacerABinTree (n `div` 2)) (hacerABinTree ((n-1) `div` 2))