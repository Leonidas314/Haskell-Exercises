{-Programación funcional, listas por comprensión, funciones de alto orden-}
-- Ejercicio1 Generar una lista infinita de unos

-- [1,1..] Es una respuesta válida
----Otra respuesta es:
--linf'::[Int]
--linf'_=[1,1..]

linf :: [Int]
linf = 1:linf

--Ejercicio 2 Genera una lista infinita de naturales comenzando desde un numero dado
linfNat :: Int -> [Int]
linfNat n = [n..]
--Alternativa:
linfNat' :: Int ->  [Int ]
linfNat' n = [n]++ linfNat'(n+1)
--Ejercicio 3 Genera una lista con los primeros n naturales

listnNat :: Int -> [Int]
listnNat n = [0..n]

--Ejercicio 4 *: Retorna los primeros 5 elementos de una lista infinita de enteros positivos

{-Ejercicio 5 : Usando una funcion de alto orden dada una lista de enteros
retornar sus cuadrados-}

listSquare :: [Int] -> [Int]
listSquare [] = []
listSquare xs = map (^2) xs

squareComprension :: [Int]->[Int]
squareComprension [] = []
squareComprension xs =[n^2|n<-xs]
--Ejercicio6 Dado un entero positivo retornar una lista con sus divisores 

listDiv :: Int -> [Int]
listDiv n = [x | x <- [1..n], mod n x == 0]
--
fdiv:: Int-> [Int]
fdiv 0=[0]
fdiv n = filter (filtro n) [1..n] where filtro x n = mod x n == 0
--Dada una lista devolver una lista solo con los numeros primos de la original
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [2..n] , mod n x == 0] == [n] 

filterPrime :: [Int]-> [Int]
filterPrime xs = filter (esPrimo) xs

--Ejercicio9 Dada un lista de naturales retornar una lista co sus succ

succList :: [Int ]-> [Int]
succList []= []
succList xs = map (+1) xs

--Ejercicio10 Sumar los elementos de una lista :usar sum
--Ejercicio 13 


tam :: [a] -> Int
tam []= 0
tam (x:xs) = foldr (succ x) 0 (x:xs) where 