--Definir la funcion nand a b = not ( a&&b ) sin utilizar not y &&

nand :: Bool -> Bool -> Bool
nand False _ = True
nand _ False = True
nand True True = False

--Definir maj que retorne True sii al menos 2 argumentos son true
maj :: Bool -> Bool -> Bool -> Bool
maj True True _=True
maj True _ True =True
maj _ True True = True
maj _ _ _ = False

--Ejercicio 4 con patern matching y recursion

existe :: Ord a => ( a -> Bool) -> [a] -> Bool
existe  f1 [] = False --Neutro
existe  f1 (x:xs) = f1  x || existe f1 xs

paratodo :: Ord a => (a-> Bool) -> [a] -> Bool
paratodo f1 [] = True 
paratodo f1 (x:xs) = f1 x && existe f1 xs

--Ejercicio 4 con and y or sobre listas de comprension
existe' :: Ord a => ( a -> Bool) -> [a] -> Bool
existe' f xs = or [f x | x <- xs]


paratodo' :: Ord a => ( a -> Bool) -> [a] -> Bool
paratodo' f xs = and [f x | x <- xs]

--Ejercicio 5 Escribir los cuantificadores de productoria sumatoria y contatoria para ejemplos concretos

--Ejemplo con pares e impares

--Sumar los pares de una lista dada
sumEven :: [Int] -> Int 
sumEven xs = sum [x | x<- xs , even x]

--Contar los impares del 1 al 25

countOdd25 :: Int
countOdd25 = sum [1 | x<-[0..25] , odd x]
