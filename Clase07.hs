--Primera Parte.
type Set a = [a]

incluye :: Int -> Set Int -> Bool
incluye n [] = False
incluye n (x:xs) = n == x || incluye n xs 

union :: Set Int -> Set Int -> Set Int
union [] b = b
union (x:xs) b| incluye x b = union xs b
              | otherwise = (union xs (x:b))

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] b = []
interseccion (x:xs) b| incluye x b = x:(interseccion xs b)
                      | otherwise = interseccion xs b

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] b = []
diferencia (x:xs) b| incluye x b = diferencia xs b
                   | otherwise = x:(diferencia xs b)

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = union (diferencia a b) (diferencia b a)

--Segunda Parte.

incluyeC n [] = False
incluyeC n (x:xs) = n == x || incluyeC n xs 


agregarTodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarTodos n [] = []
agregarTodos n (x:xs) = (union [n] x):(agregarTodos n xs)

unionConjuntos :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionConjuntos [] b = b
unionConjuntos (x:xs) b| incluyeC x b = unionConjuntos xs b
               | otherwise = (unionConjuntos xs (x:b))

partesN :: Int -> Set (Set Int)
partesN 0 = [[]] 
partesN n = unionConjuntos (partesN (n-1)) (agregarTodos n (partesN (n-1)))


unionC :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
unionC [] b = b
unionC (x:xs) b| incluyeC x b = unionC xs b
               | otherwise = (unionC xs (x:b))
agregarTupla :: Int -> Set Int -> Set (Int, Int)
agregarTupla n [] = []
agregarTupla n c = (n, (head c)): agregarTupla n (tail c)

productoCartesiano ::  Set Int  -> Set Int -> Set (Int, Int)
productoCartesiano [] _ = []
productoCartesiano (x:xs) b = unionC (agregarTupla x b) (productoCartesiano xs b)

--NOTA: SEGUNDA PARTE HECHA SIN NADA DE ONDA.