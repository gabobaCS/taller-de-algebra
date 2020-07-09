type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

agregarEnTodas :: Eq a => a -> Set [a] -> Set [a]
agregarEnTodas n [] = []
agregarEnTodas n xs = union [n:(head xs)] (agregarEnTodas n (tail xs))

agregarElementosLista :: Eq a => Set a -> Set [a] -> Set [a]
agregarElementosLista [] _ = []
agregarElementosLista xs c = union (agregarEnTodas (head xs) c) (agregarElementosLista (tail xs) c)

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas 0 _ = [[]]
bolitasEnCajas n k = (agregarElementosLista [1,2..k] (bolitasEnCajas (n - 1) k))

numeroEnLista :: Int -> Set Int -> Bool
numeroEnLista n [] = False
numeroEnLista n xs = n == (head xs) || numeroEnLista n (tail xs)

bolitasEnCajaNoVaciaAUX :: Set [Int] -> Set [Int]
bolitasEnCajaNoVaciaAUX [] = []
bolitasEnCajaNoVaciaAUX xss| numeroEnLista 1 (head xss) =  (head xss): bolitasEnCajaNoVaciaAUX (tail xss)
                           | otherwise = bolitasEnCajaNoVaciaAUX (tail xss)

bolitasEnCajaNoVacia :: Int -> Int -> Set [Int]
bolitasEnCajaNoVacia n k = bolitasEnCajaNoVaciaAUX (bolitasEnCajas n k)

resta :: Eq a => a -> Set a -> Set a
resta n [] = []
resta n xs| n == (head xs) = resta n (tail xs)
          | otherwise = (head xs): resta n (tail xs)

separador [] = []
separador (x:xs) = [x]:(separador xs)

-- listaOrdenadaAux :: Set Int -> Int -> Int  
listaOrdenadaAux xs n [] = []
listaOrdenadaAux 0 n list = []
listaOrdenadaAux target n list = agregarEnTodas target ((target `resta` list) `union` listaOrdenadaAux (target - 1) n (list))
