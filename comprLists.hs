--En este archivo se resolveran problemas solo con listas de comprension
--Con el fin de entender su funcionamiento y potencial.

--Ejercicio 1 a: Crear un lista con los 10 primeros numeros enteros

ejer1a :: [Int]
ejer1a =  [x | x <- [0..9]]

--Ejercicio 1 b: Crear una lista con los 10 primeros numeros pares

ejer1b :: [Int]
ejer1b = [x | x<-[0,2..18]]

--Ejercicio 1 c: Crear una lista con todos los impares menores que un n dado

ejer1c :: Int -> [Int]
ejer1c n = [x | x<-[0..n], not (x`mod`2==0)]
--ejer1c n = [x*2+1 | x <- [0..n] , x<n] Error!, aqui creamos una lista con todos los x menores a n y luego a esos x les aplicamos x*2+1. Lo correcto es especificar que x*2+1 < n como se decribe a cuntinuacion:

ejer1cA :: Int -> [Int]
ejer1cA n = [x*2+1| x<-[0..n] , x*2+1<n]

--Las expresiones booleanas luego de la coma se denominan predicados o filtros, ya que en la lista que creamos con ellos solo entran los que cumplen con dicha expresion.

--Ejemplo de funcion que recibe una lista y reemplaza todos los numeros impares con el string "Boom!" si son menores que 10 y con "Bang!" si son mayores que 10.

boombang :: [Int] -> [String]
boombang xs = [if x<10 then "Boom!" else "Bang!"|x<-xs , odd x]

--Ejercicio 2 : Crear una lista de numeros primos hasta n

--Funcion isprime: Confirma si la lista generada contiene solo como divisor al numero ingresado, es decir , si el numero ingresado es su unico divisor es primo
isprime :: Int -> Bool
isprime n = [x | x <- [2..n], n`mod`x==0]==[n]

listPrimes :: Int -> [Int]
listPrimes n = [x | x <- [2..n], isprime x]

--repaso de substring
prefix :: Eq a => [a]->[a] -> Bool
prefix [] _ = True
prefix [x] [] = False
prefix (x:xs) (y:ys) = x==y && prefix xs ys

substring :: Eq a => [a] -> [a] -> Bool
substring [] _ = True
substring (x:xs) [] = False
substring (x:xs) (y:ys) = prefix (x:xs) (y:ys) || substring (x:xs) ys

--Crear funcion allOcurrIn que dadas dos listas confirma si todos los elementos de la primera lista ocurren en la segunda

allOcurrIn :: Eq a => [a] -> [a] -> Bool
allOcurrIn (xs) (ys) = [z | z <- xs , not (elem z ys) ] == []

--Esta funcion toma dos listas y luego crea una tercera con los elementos de xs tales que no se encuentre en ys, si esta lista es vacia entonces todos los elementos de xs se encuentran en ys

--Crear la funcion split2 que dada una lista devuelva todas las formas de partir en dos a la misma en una tupla

split2 :: [a] -> [([a],[a])]
split2 xs = [(take i xs, drop i xs)| i <- [0.. length xs]]

--Crear la funcion split3 que dada una lista devuelva todas las formas de partila en tres en una tri-upla

takeAndDrop :: Eq a => Int -> [a] -> [a]
takeAndDrop j xs 
 |j == 0 || j==length xs = []
 |j < length xs = take j xs
 |j > length xs = drop (j-length xs) xs

split3 :: Eq a => [a] -> [([a],[a],[a])]
split3 xs = [(take i xs,takeAndDrop j xs,drop i xs) | i <- [0..length xs], j<-[0..2*length xs], j==i || j == 2*i]