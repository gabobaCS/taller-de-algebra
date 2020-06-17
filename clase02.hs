estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y = (x <= 3 && y <= 3) || (3 < x  &&  x <= 7 && 3 < y && y <= 7) || ( x > 7 && y > 7)

prodInt :: (Float, Float) -> (Float, Float)  -> Float
prodInt (a, b) (x, y) = a*x + b*y

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a, b) (x, y) = a < b && x < y 

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a, b) (x, y) = sqrt((a-x)^2 + (b - y)^2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a + b + c

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (a, b, c)| (a `mod` 2 == 0) && (b `mod` 2 == 0) && (c `mod` 2  == 0) = 4
                        | a `mod` 2 == 0 = 0
                        | b `mod` 2 == 0 = 1
                        | c `mod` 2 == 0 = 2

crearPar :: a -> b -> (a,b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)
