geo :: Int -> Int -> Int
geo i 1 = i
geo i n = i^n + geo i (n-1)

g1 :: Int -> Int -> Int
g1 i n = (geo i n) - (geo i (i-1))

g2Aux :: Int -> Int -> Int
g2Aux 1 n = 1
g2Aux i n = i^n + (g2Aux (i-1) n)

g2 :: Int -> Int
g2 1 = 1
g2 n = (g2Aux n n) + (g2 (n-1))

g3 :: Int -> Int
g3 1 = 0
g3 n| n `mod` 2 == 0 = 2^n + (g3 (n-1))
    | otherwise = (g3 (n-1))

digitosIguales :: Int -> Bool
digitosIguales n| n < 10 = True
                | otherwise = n `mod` 10 == (n `div` 10) `mod` 10 && digitosIguales(n `div` 10)

sumaIguales :: Int -> Int
sumaIguales 1 = 1
sumaIguales n| (digitosIguales n) = n + sumaIguales(n-1)
             | otherwise = sumaIguales(n-1)
