--Subsegmento de suma minima (devuelve siempre cero)
g::[Int] -> Int
g []= 0
g (x:xs) = 0 `min` (x + g xs)

f::[Int] -> Int
f [] = 0
f (x:xs) = (x + g xs) `min` f xs 

--Subseg suma cero

f0 :: [Int] -> Bool
f0 [] = False
f0 (x:xs) = g0 (x:xs) || (f0 xs) where g0 (x:xs) = x==0 || sum xs == 0 

subseg::String -> String -> Bool
subseg [] _ = True
subseg _ [] = False
subseg (x:xs) (y:ys) = y==x && subsegIn xs ys || subseg (x:xs) ys where subsegIn [] _ = True
                                                                        subsegIn _ [] = False
                                                                        subsegIn (x:xs) (y:ys)= y==x && subsegIn xs ys

--Funcion que devuelve True si alguno de los elementos es igual a la suma de los anteriores

f2 :: [Int]->Bool
f2 [] = False
f2 (x:xs) = x == 0 || (q 0 (x:xs)) where q n [] = False
                                         q n (x:xs) = x==n || q (n+x) xs

--Funcion que recibe una lista de naturales y un natural "k" y determina si todos los numeros en las posiciones mayores a la k-esima pocision son mayores al de 
-- la k-esima posicion

f3 :: [Int] -> Int -> Bool
f3 [] _ = True
f3 [x] _ = True
f3 (x:y:xs) k = (y:xs)!!k < (x:y:xs)!!k && f3 (y:xs) (k)