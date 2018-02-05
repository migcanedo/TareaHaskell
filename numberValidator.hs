{-
Ejercicio #1 de La Tarea 1 sobre Haskell

Autores: Andres Buelvas 13-10184
		 Miguel Canedo 13-10214

Fecha: 04/02/18
-}

{-
Descripcion:
		Funcion que transforma un numero entero positivo
		en una lista con cada uno de sus digitos.
-}
aDigits :: Integer -> [Integer]
aDigits n
	| n <= 0 = []
	| n < 10 = [n]
	| otherwise = aDigits (div n 10) ++ [mod n 10]

{-
Descripcion:
		Funcion que transforma un numero entero positivo
		en una lista con cada uno de sus digitos en orden
		invertido.
-}
aDigitsRev :: Integer -> [Integer]
aDigitsRev x = reverse . aDigits $ x

{-
Descripcion:
		Funcion que duplica cada segundo elemento de una lista, 
		recorriendola desde el final hasta el inicio.
-}
duplicarCadaOtro :: [Integer] -> [Integer]
duplicarCadaOtro [] = []
duplicarCadaOtro [p] = [p]
duplicarCadaOtro xs = (duplicarCadaOtro . init . init $ xs) ++ [(last . init $ xs)*2] ++ [last xs]

{-
Descripcion:
		Funcion que calcula la suma de cada uno de los digitos 
		de una lista de enteros positivos.
-}
sumDigitos :: [Integer] -> Integer
sumDigitos [] = 0
sumDigitos (p:resto) = sum (aDigits p) + sumDigitos resto

{-
Descripcion:
		Funcion que, haciendo uso de la sfunciones previamente definidas,
		realiza la validacion de un numero de tarjeta.
-}
validar :: Integer -> Bool
validar x = sumDigitos (duplicarCadaOtro (aDigits x)) `mod` 10 == 0