-- NumberValidator.hs
--
-- Autores: Andres Buelvas
--			Miguel Canedo

-- Descripcion:
aDigits :: Integer -> [Integer]
aDigits n
	| n < 0 = error "Error. No funciona con Numeros Negativos" 
	| n < 10 = [n]
	| otherwise = aDigits (div n 10) ++ [mod n 10]

-- Descripcion:
aDigitsRev :: Integer -> [Integer]
aDigitsRev x = reverse (aDigits x)

-- Descripcion:
duplicarCadaOtro :: [Integer] -> [Integer]
duplicarCadaOtro [] = []
duplicarCadaOtro [p] = [p]
duplicarCadaOtro (p:s:resto) = [p] ++ [s*2] ++ duplicarCadaOtro resto

-- Descripcion:
sumDigitos :: [Integer] -> Integer
sumDigitos [] = 0
sumDigitos (p:resto) = sum (aDigits p) + sumDigitos resto

-- Descripcion: 
validar :: Integer -> Bool
validar x = sumDigitos (duplicarCadaOtro (aDigitsRev x)) `mod` 10 == 0