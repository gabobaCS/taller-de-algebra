absoluto :: Int -> Int 
absoluto x | x < 0 = (-x)
           | otherwise = x

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
             | otherwise = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | otherwise = z

algunoEs0 :: Float -> Float -> Bool
-- algunoEs0 x y = (x == 0 || y == 0)
algunoEs0 0 _ = True
algunoEs0 x y = y == 0

ambosSon0 :: Float -> Float -> Bool
-- ambosSon0 x y = (x == 0 && y == 0)
ambosSon0 0 0 = True
ambosSon0 _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10