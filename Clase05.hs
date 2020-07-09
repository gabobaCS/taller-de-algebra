module Clase05 where
-- Problema #9
fibonacci :: Int -> Int
fibonacci n| n == 1 || n == 2 = 1
           |otherwise = fibonacci (n-1) + fibonacci (n-2)


esFibonacciHasta :: Int -> Int -> Bool
esFibonacciHasta n m| n <= fibonacci m = n == fibonacci m
                    | otherwise = n == fibonacci m || esFibonacciHasta n (m+1)

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciHasta n 1

-- Problema #10

esPrimoDesde :: Int -> Int -> Bool
esPrimoDesde n m | n == m = n /= 1
                 | m == 1 = esPrimoDesde n (m+1)
                 |otherwise = n `mod` m /= 0 && esPrimoDesde n (m+1)

esPrimo :: Int -> Bool 
esPrimo n = esPrimoDesde n 1

sumaPrimerosPrimos :: Int -> Int -> Int -> Int
sumaPrimerosPrimos n m o| n == 0 || n == 1 && esPrimo m = o + m
                        |esPrimo m = sumaPrimerosPrimos (n-1) (m+1) (o+m)
                        |otherwise = sumaPrimerosPrimos n (m+1) o 

sumaPrimerosPrimosIterar :: Int -> Int -> Bool
sumaPrimerosPrimosIterar n m| n < sumaPrimerosPrimos m 0 0 = False 
                            | n == sumaPrimerosPrimos m 0 0 = True 
                            | otherwise = sumaPrimerosPrimosIterar n (m+1)

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = sumaPrimerosPrimosIterar n 0

-- Problema #11

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n m| m == 1 = 1
                      | n `mod` m == 0 = m + sumaDivisoresHasta n (m-1)
                      | otherwise = sumaDivisoresHasta n (m-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

tomaValorMaxAux :: Int -> Int -> Int -> Int
tomaValorMaxAux n m o| n <= m = o
                     | sumaDivisores n > sumaDivisores o = tomaValorMaxAux (n-1) m n
                     | otherwise = tomaValorMaxAux (n-1) m o

tomaValorMax :: Int -> Int -> Int 
tomaValorMax n m = tomaValorMaxAux n m 1

-- Problema #12

tomaValorMinAux :: Int -> Int -> Int -> Int
tomaValorMinAux n m o| n <= m = o
                     | sumaDivisores n < sumaDivisores o = tomaValorMinAux (n-1) m n
                     | otherwise = tomaValorMinAux (n-1) m o
                     
tomaValorMin :: Int -> Int -> Int 
tomaValorMin n m = tomaValorMinAux n m (sumaDivisores n)

-- Problema #13

todasSumasDePrimosN :: Int -> Int -> Int -> Bool
todasSumasDePrimosN 1 m o = False
todasSumasDePrimosN n m o| not(esPrimo m) = False
                         | not(esPrimo n) = todasSumasDePrimosN (n-1) m o
                         | otherwise = n + m == o || todasSumasDePrimosN (n-1) m o

todasSumasDePrimosM :: Int -> Int -> Int -> Bool
todasSumasDePrimosM n 1 o = False
todasSumasDePrimosM n m o| not(todasSumasDePrimosN n m o) = todasSumasDePrimosM n (m-1) o
                         | otherwise = True

esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n = todasSumasDePrimosM n n n

-- Problema #14 - Trivial con Problema #13

-- Problema #15
primosGemAux :: Int -> Int -> Int
primosGemAux 1 m = m
primosGemAux n m| esPrimo(n - 2) && esPrimo(n) = primosGemAux (n-1) (m+1)
                | otherwise = primosGemAux (n-1) m

primosGem :: Int -> Int
primosGem n = primosGemAux n 0

--Problema #16
proxPrimosGem :: Int -> (Int,Int)
proxPrimosGem n| esPrimo(n+1) && esPrimo(n+3) = (n+1, n+3)
               |otherwise = proxPrimosGem (n+1)

--Problema #17
largoSecuenciaAux :: Int -> Int -> Int
largoSecuenciaAux 1 m = m
largoSecuenciaAux n m| n `mod` 2 == 0 = largoSecuenciaAux (n `div` 2) (m+1)
                     | otherwise = largoSecuenciaAux (3*n + 1) (m+1)

largoSecuencia :: Int -> Int
largoSecuencia n = largoSecuenciaAux n 0

maxCollatz :: Int -> Int -> Int
maxCollatz 10000 m = m
maxCollatz n m| largoSecuencia n > largoSecuencia m = maxCollatz (n+1) n
              | otherwise = maxCollatz (n+1) m

-- Haciendo maxCollatz 1 1 me da 6171