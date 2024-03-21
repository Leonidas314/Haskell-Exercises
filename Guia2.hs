--Ejercicio2

--head retorna el primer elemento de una lista
head' :: [a] -> a
head' (x:xs) = x 
--tail retorna toda la lista menos el primer elemento
tail' :: [a] -> [a]
tail' (x:xs) = xs
--last' retorna el ultimo elemento
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

--init' retorna todos los elementos de la lista menos el ultimo
init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = [x] ++ init' xs

--Ejercicio 3 obtener el maximo de tres interos
maxTres :: Int -> Int -> Int -> Int
maxTres x y z 
 | x >= y && x >= z = x
 | y >= z && y >= x = y 
 | otherwise = z

absolute :: Int -> Int
absolute x 
 | x>=0 = x 
 |otherwise = -x

concatenar :: [a] -> [a] -> [a]
concatenar a b = a ++ b

tomar :: Int -> [a] -> [a]
tomar 0 (x:xs) = []
tomar n (x:xs) = [x] ++ (tomar (n-1) (xs))

dejar :: Int -> [a] -> [a]
dejar 0 (x:xs) = [x] ++ xs
dejar n (x:xs) = [] ++ dejar (n-1) (xs)

edad :: (Num a) => (a,a,a) -> (a,a,a) -> a
edad (d1,m1,a1) (d2,m2,a2)  = a1-a2

edad' :: Int  => (a,a,a) -> (a,a,a) -> a
edad' (d1,m1,a1) (d2,m2,a2)  = a1-a2
