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
--Ejercicio 4 definir las funciones concatenar, tomar , dejar y agregar al final
concatenar :: [a] -> [a] -> [a]
concatenar a b = a ++ b

tomar :: Int -> [a] -> [a]
tomar 0 (x:xs) = []
tomar n (x:xs) = [x] ++ (tomar (n-1) (xs))

dejar :: Int -> [a] -> [a]
dejar 0 (x:xs) = [x] ++ xs
dejar n (x:xs) = [] ++ dejar (n-1) (xs)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal lista elemento  = lista ++ [elemento] 

--Ejercicio 5 definir funcion abs 
absolute :: Int -> Int
absolute x 
 | x>=0 = x 
 |otherwise = -x
--Ejercicio 6 (Pendiente)
edad :: (Num a) => (a,a,a) -> (a,a,a) -> a
edad (d1,m1,a1) (d2,m2,a2)  = a1-a2

--Ejercicio 7 : Definir  la funcion xor : disyunción exclusiva
xor :: Bool -> Bool -> Bool 
xor False x = x
xor x False = x 
xor True True = False
--Ejemplo De funcion de conjuncion logica
conjuncion :: Bool -> Bool -> Bool
conjuncion False x = False  
conjuncion True x = x 
 --Ejercicio8 Defina una fucion que dado un numero natural defina si es primo o no
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [2..n] , mod n x == 0] == [n] 

biImplication :: Bool -> Bool -> Bool
biImplication p q = p==q