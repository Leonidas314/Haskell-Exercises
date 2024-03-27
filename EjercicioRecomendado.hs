--Definir una funcion que toma una secuencia y decir si todos sus digitos son pares
secuenciaPar :: [Int] -> Bool
secuenciaPar [] = True
secuenciaPar (x:xs) = (mod x 2 == 0) && (secuenciaPar (xs))


--Alternativa combinando pattenr matching y guards
secuenciaPar' :: [Int] -> Bool
secuenciaPar' [] = True 
secuenciaPar' (x:xs)
 | ([x]==[]) = True
 | ((mod x 2) == 0) = secuenciaPar'(xs)
 | otherwise = False 

--Ordenar una lista probar selection sort, elegir el mas chico y ponerlo primero

selectionSort ::(Eq a, Ord a)=>[a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort (x:y:xs)
 |x>y = selectionSort (y:x:xs)
 |x<y = x : selectionSort(y:xs)
 |x>y && xs ==[] = y : [x]
 |x<y && xs == [] = x : [y]

--Define a function that gives the  maximun of a list of integers. Use the predefined function max
maxOfaList :: [Int] -> Int
maxOfaList [] = 0
maxOfaList [x] = x
maxOfaList (x:xs) = max x (maxOfaList xs) 

--Define a function that returns the first ocurrence of an integer m in a list of integers

firstOcurrence :: Int ->[Int] -> Int
firstOcurrence m [] = error "empty list"
firstOcurrence m (x:xs) 
 | m==x =  x
 | otherwise = firstOcurrence m xs

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

--Exercise 1.13 Write a function count for counting the number of ocurrences of a caracter in a string

count :: Char -> [Char] -> Int
count c [] = 0
count c (x:xs)
 | c == x = 1 + count c xs
 | otherwise = count c xs

--Exercise 1.14 Write a function that take a string and returns same list but all its elements are tripicated  except the firstone

triplicate :: String -> String 
triplicate [] = []
triplicate (x:xs) = x : x : x : triplicate xs

blowup :: String -> String 
blowup (x:xs) = [x] ++ triplicate xs  

--Exercise 1.15 Write a function srtString that sorts a sring in alphabetical order
--For this we need to obtain te minimun Char in a list
--And we modify the removeFirst's identity to a polimorfic function
minChar ::(Ord a ) => [a]-> a 
--minChar [] = undefined
minChar [x] = x
minChar (x:xs) = min x (minChar xs)

srtString ::(Eq a, Ord a) => [a] -> [a]
srtString [] = []
srtString (x:xs)= m : (srtString (removeFirst (m) (xs))) where m = minChar xs