{-Programación funcional, listas por comprensión, funciones de alto orden-}
-- Ejercicio1 Generar una lista infinita de unos

-- [1,1..] Es una respuesta válida
----Otra respuesta es:

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
primerosNelem :: Int -> Int -> [Int]
primerosNelem m n = take n (linfNat m)

{-Ejercicio 5 : Usando una funcion de alto orden dada una lista de enteros
retornar sus cuadrados-}

listSquare :: [Int] -> [Int]
listSquare [] = []
listSquare xs = map (^2) xs


listDiv :: Int -> [Int]
listDiv n = [x | x <- [1..n], mod n x == 0]
--
fdiv:: Int-> [Int]
fdiv 0=[0]
fdiv n = filter (filtro n) [1..n] where filtro x n = mod x n == 0
--Ejercicio 7 Dada una lista devolver una lista solo con los numeros primos de la original

esPrimo :: Int -> Bool
esPrimo n = [x | x <- [2..n] , mod n x == 0] == [n] 

filterPrime :: [Int]-> [Int]
filterPrime xs = filter (esPrimo) xs

--Ejercicio9 Dada un lista de naturales retornar una lista co sus succ

succList :: [Int ]-> [Int]
succList []= []
succList xs = map (+1) xs


--Ejercicio8 Dada una lista de nats retornar la suma de los cuadrados

sumSquare :: ([Int] -> [Int] )-> [Int] -> Int
sumSquare lsqr xs = sum (lsqr xs)

--Ejercicio 10 Dada una lista de enteros sumar todos sus elementos 

sumElem :: [Int]-> Int
sumElem []=0
sumElem [x]=x
sumElem (x:xs)= x + sumElem(xs)

--Ejercicio11 Definir el factorial usando fold
--foldr
factFoldr :: Int -> Int
factFoldr n = foldr (*) (1) ([1..n])
--foldl
factFoldl :: Int -> Int
factFoldl n = foldl (*) 1 [1..n]
--El producto es asociativo y conumtativo entonces el resultado es el mismo

--Ejercicio12 Definir una funcion "and" tal que and xs verifique si todos los elementos de xs son verdaderos

and :: [Bool] -> Bool
and xs = foldr (&&) True xs 

--Ejercicio 13 Definir la funcion tam que recibe una lista y devuelve el largo de la misma, usar foldr
--foldr :: (a -> b -> b) -> b -> t a -> b

--Creo una funcion binaria que sume 
sumPos :: [a]-> b -> Int
sumPos [] _= 0
sumPos [x] _ = 1
sumPos (x:xs) a = 1 + sumPos xs a

tam :: [a] -> Int
tam lista = foldr (sumPos) 0 [lista]


--Cuando las funciones son asociativas y conmutativa es indiferente el uso de foldl y foldr, en el caso de la resta por ejemplo no es lo mismo

--Funcionamiento interno de foldr
-- 1:2:3:4: (..n) al final el acumulador
--Funcionamiento con foldl
--[] ++ [a1] ++[a2] ++ [a3]
--Pruebas con foldl y foldr en listas :


--Utilizando listas por comprension resolver 
--Ejercicio 14 Dada una lista de enteros, retornar sus sucesores

succList' :: [Int] -> [Int]
succList' xs = [x+1 | x <- xs, even x]
--Toma los pares y te devuelve los sucesores de los pares , osea digamos los impares

--Ejercicio 15 Dada una lista de naturales ,retornar sus cuadrados

squareComprension :: [Int]->[Int]
squareComprension xs =[n^2|n<-xs]
--Ejercicio6 Dado un entero positivo retornar una lista con sus divisores 

--Ejercicio16 Dada una lista de enteros retornar los elementos pares que sean mayores a 10 

listEventen :: [Int] -> [Int]
listEventen xs = [n | n <- xs , even n ,n>10 ]
--Esta sentecencia se lee . n tal que n pertenece a xs y n es par y n es mayor a 10

--Ejercicio 17 Dado un entero retornar sus divisores 

divisoresN :: Int -> [Int]
divisoresN n = [x | x <- [1..n], mod n x==0]

--Ejercicio18 Definir la funcion 
--todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
--tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos de ys 
--todosOcurrenEn [1,5,2,5] [5,1,2,4] = True
--todosOcurrenEn [1,5,2,5] [5,2,4] = False


existElem :: Eq a => a -> [a] -> Bool
existElem _ []= False
existElem x (y:ys) = (x==y) || existElem x ys

toEn :: Eq a => [a] -> [a] -> Bool
toEn [] _ = True 
toEn (x:xs) (ys) = existElem x ys && toEn xs ys 
