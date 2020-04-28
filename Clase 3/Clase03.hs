tribonacci :: Int -> Int
tribonacci n | n <= 2 = n
             | otherwise = tribonacci(n - 1) + tribonacci(n - 2) + tribonacci(n - 3) 

esMultiploDe3 :: Int -> Bool
esMultiploDe3 3 = True
esMultiploDe3 n = n > 0 && esMultiploDe3 (n - 3)

diabolico :: Int -> Bool
diabolico 6 = True
diabolico n = n > 0 && diabolico(n `div` 10) && diabolico(n `mod` 10)


digitosIguales :: Int -> Bool
digitosIguales n| n < 10 = True
                | otherwise = (n `mod` 10) == (n `div` 10) `mod` 10 && digitosIguales(n `div` 10)

resta :: Int -> Int -> Int
resta 0 _ = 0
resta n 0 = n
resta n m = resta (pred n) (pred m)  

menor :: Int -> Int -> Bool
menor 0 0 = False
menor 0 m = True
menor n 0 = False
menor n m = menor (resta n m ) (resta m n)

mayor :: Int -> Int -> Bool
mayor 0 0 = False
mayor n 0 = True
mayor 0 m = False
mayor n m = mayor (resta n m ) (resta m n)

iguales :: Int -> Int -> Bool
iguales n m = not (mayor (resta n m ) (resta m n)) && not (menor (resta n m ) (resta m n))

esPotencia :: Int -> Int -> Bool
esPotencia n m =  n == 1 || n >= m &&  esPotencia (div n m) m && n `mod` m == 0