import Data.Char
import Data.IntSet
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
removeFirst :: Int -> [Int] -> [Int]
removeFirst m [] = []
removeFirst m (x:xs)  |m==x = xs | otherwise = x : removeFirst m xs 

--minimo de una lista de integers

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

srtInts :: [Int]->[Int]
srtInts []=[]
srtInts xs = m : (srtInts (removeFirst (m) (xs))) where m = mnmInt xs 