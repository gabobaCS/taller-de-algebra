productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n xs = (head xs + n):(sumarN n (tail xs))

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

ultimoElemento :: [Int] -> Int
ultimoElemento l | tail l == [] = head l
                 | otherwise = ultimoElemento (tail l)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (ultimoElemento xs) xs

pares :: [Int] -> [Int]
pares [] = []
pares xs| mod (head xs) 2 == 1 = pares (tail xs)
        | otherwise = (head xs):(pares (tail xs))

quitar :: Int -> [Int] -> [Int]
quitar n xs| n == head xs = tail xs
           | otherwise = (head xs):(quitar n (tail xs))

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n xs| n == head xs =  quitarTodas n (tail xs)
                | otherwise = (head xs):(quitarTodas n (tail xs))

numeroApariciones :: Int -> [Int] -> Int
numeroApariciones _ [] = 0
numeroApariciones n xs| n == head xs = 1 + numeroApariciones n (tail xs)
                      | otherwise = numeroApariciones n (tail xs)

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos xs = (numeroApariciones (head xs) xs > 1) || hayRepetidos (tail xs)

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal xs = (head xs) : eliminarRepetidosAlFinal (quitarTodas (head xs) (tail xs))

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = [] 
eliminarRepetidosAlInicio xs| numeroApariciones (head xs) xs > 1 = eliminarRepetidosAlInicio (quitar (head xs) xs)
                            | otherwise = (head xs) : eliminarRepetidosAlInicio (tail xs)

maximo :: [Int] -> Int
maximo xs|  tail xs == [] = head xs
         | (head xs) > maximo (tail xs) = head xs
         | otherwise = maximo (tail xs)

minimo :: [Int] -> Int
minimo xs|  tail xs == [] = head xs
         | (head xs) < minimo (tail xs) = head xs
         | otherwise = minimo (tail xs)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar xs = (minimo xs) : ordenar(quitar (minimo xs) xs)

quitarUltimo :: [Int] -> [Int]
quitarUltimo xs| tail xs == [] = []
               |otherwise = (head xs) : quitarUltimo (tail xs)

reverso :: [Int] -> [Int]
reverso [] = []
reverso xs = (ultimoElemento xs) : reverso(quitarUltimo xs)

concatenar :: [Int] -> [Int] -> [Int]
concatenar _ [] = []
concatenar xs ys| xs /= [] = (head xs ): concatenar (tail xs) ys
                | otherwise = (head ys ): concatenar xs (tail ys)

zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi xs ys = ((head xs), (head ys)):(zipi (tail xs) (tail ys))

