module Clase09 where
--Parte II

digitos :: Integer -> Integer -> [Integer]
digitos 0 b = []
digitos n b = (n `mod` b):(digitos (n `div` b) b)

numero :: [Integer] -> Integer -> Integer
numero [] b = 0
numero representacion b = (b^( (length representacion) - 1))*(last representacion) + (numero (init representacion) b)

--Parte III
type Set a = [a]

divisoresAux :: Int -> Int -> Set Int -> Set Int
divisoresAux n counter list| counter > n = list
                           | n `mod` counter == 0 = divisoresAux n (counter + 1) (counter:list)
                           | otherwise = divisoresAux n (counter + 1) list

divisores :: Int -> Set Int
-- dado un valor n != 0 retorna  retorna el conjunto de sus divisores positivos
divisores n = divisoresAux n 1 []

incluye :: Eq a => Set a -> a -> Bool
incluye [] a = False
incluye (x:xs) a| x == a = True
              | otherwise = False || incluye xs a  

interseccion :: Eq a => Set a -> Set a -> Set a
interseccion [] ys = []
interseccion (x:xs) ys| incluye ys x = x:(interseccion xs ys)
                    | otherwise = interseccion xs ys

maximo :: Set Int -> Int
maximo xs|  tail xs == [] = head xs
         | (head xs) > maximo (tail xs) = head xs
         | otherwise = maximo (tail xs)

mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo ( interseccion ( divisores a) ( divisores b ))

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd (abs b) ( abs a `mod` abs b)

mcm :: Int -> Int -> Int
mcm a b = (a*b) `div` (mcd a b)

--Parte IV

emcd :: Int -> Int -> (Int, Int, Int)
emcd a 0 = (a, 1, 0)
emcd a b = (d, t, s - (a `div` b)*t)
    where
        (d,s,t) = emcd b (a `mod` b) 

  
--FALTA NUMERO 10