--Funcion que determina si todos los elementos de una lista son iguales 
f1 :: (Eq a) =>[a] -> Bool
f1 (x:xs) = g x xs where g _ [] = True
                         g (y) (z:zs) = (y == z) && (g z zs)


--Funcion que dada una lista determina si sus elementos estan ordenados de forma creciente
f2 :: [Int] -> Bool
f2 [] =True
f2 [x] = True
f2 (x:y:xs) = y<=x && f2 xs
--Funcion que dada una lista determina si alguno de sus elemtos es igual a la suma del resto de la lista
g3 :: Int ->[Int] -> Bool
g3 _ [] = False
g3 k (x:xs) = k + sum xs == x || g3 (k+x) xs