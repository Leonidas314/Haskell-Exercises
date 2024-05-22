{-The objective of the next guide is to reinforce the knowledge of functional
programming . Each exercise need to have the profile's function-}

{-Exercise 1 : Defina a function that recibe two list of integers already orderedand in increasing way and retorn the merge of the list xs , ys-}

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs 
merge [] ys = ys 
merge (x:xs) (y:ys) 
 | x<= y = x : merge xs (y:ys)
 | y<= x = y : merge (x:xs) ys
 | (x:xs)== [] = merge [] (y:ys)
 | (y:ys)== [] = merge (x:xs) [] --Ready to go!

--Exercise 2 Define a function that order a list of ints in increasing way 
--Auxiliar functions next:
--Define a function that removes the first ocurrence of an integer m
removeFirst ::(Eq a ) => a -> [a] -> [a]
removeFirst m [] = []
removeFirst m (x:xs)  |m==x = xs | otherwise = x : removeFirst m xs 

--minimo de una lista de integers

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

--Funcion que recibe una lista de enteros (desordenada) y devuelve la lista ordenada de forma creciente
--La forma de ordenamiento es similar al algoritmo SelectionSort
srtInts :: [Int]->[Int]
srtInts []=[]
srtInts xs = m : (srtInts (removeFirst (m) (xs))) where m = mnmInt xs 

--Exercise 3 Define a function that, recursibly , and only using adition and multiplication,calules ,given a natural n, 2^n

pow2 :: Int -> Int 
pow2 0 = 1
pow2 1 = 2
pow2 (n) = 2 * pow2(n-1) 


--Introduction to Mathematical Induction
sumOdds' :: Integer -> Integer
sumOdds' n = sum [2*k - 1 | k <- [1..n]]

sumOdds :: Int -> Int
sumOdds n = n^2

--Exercise 4_a)Definir una funcion que dado un numero natural n, retorne su representación binaria como secuencia de bits ( 4_b) resolver con foldl)

intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin 1 = [1] 
intToBin n = intToBin (div n 2) ++ [mod n 2]

--Exercise 5 Definir una funcion que reciba un numero binario y devuelva su representacion decimal _b decida si n es par o no

binToInt :: [Int] -> Int
binToInt [] = 0
binToInt [0]= 0
binToInt [1]= 1
binToInt (x:xs)= x*2^((length (x:xs))-1) + binToInt (xs)

esparBin :: [Int] -> Bool
esparBin [0]=True
esparBin [1]=False
esparBin (x:xs)= esparBin xs

--Exercise 6 define una funcion que retorne la distancia de Hamming: dadas dos listas es el numero de posiciones en que los correspondientes elementos son distintos

distanciaH :: String -> String -> Int
distanciaH [] [] = 0
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys) 
 |x == y = 1 
 |otherwise = 1 + distanciaH xs ys 

 --Exercise 7 Define la funcion que dado un número natural, decida si el mismo es un cuadrado perfecto o no

square :: Float -> Bool
square n = sqrt n - fromIntegral(floor ( sqrt n))  == 0

--Exercise 8 Define una funcion de forma tal que dado un elemento z y un entero n, z aparece n veces en donde?

repetidos :: a -> Int -> [a]
repetidos _ 0 = []
repetidos z n = z: repetidos z (n-1)

--Exercise 9 Define la funcion nelem tal que nelem xs n es elemento en-ésimo de xs empezando a numerar desde el cero. por ejemplo 

nelem :: [a] -> Int -> a
nelem [] _ = error "Lista Vacia"
nelem (x:xs) 0 = x
nelem (x:xs) n 
 | n >= length (x:xs) = error "Posicion invalida"
 | otherwise = nelem xs (n-1)

--Exercise 10 Define la funcion posicionesC tal que posicionesC xs c es la lista de las posiciones del caracter c een la cadena xs.

posicionesC :: (Eq a) => [a] -> a -> [Int]
posicionesC [] _ = []
posicionesC (x:xs) c 
 | x==c = 0 : map (+1) (posicionesC xs c) 
 | otherwise = map (+1) (posicionesC xs c)

--Exercice 11 Defina la funcion compact, dad una lista retorna la lista sin los elementos repetidos consecutivos 

compact :: Eq a => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:xs)
 |x==y && xs ==[] = x:compact [] 
 |x/=y && (xs==[]) = x:[y]
 |x==y = compact (x:xs) 
 |otherwise = x:compact (y:xs)

--Hacer Quick sort
--1)Elegir un pivote: comunmente el primer elemento
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] =[x]
quicksort (x:y:xs) 
 | y<=x =  y : quicksort (x:xs)
 | otherwise = quicksort(x:xs)++[y]

