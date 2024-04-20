--Define a function that evalues if all the elements of a list are even numbers
secuenciaPar :: [Int] -> Bool
secuenciaPar [] = True
secuenciaPar (x:xs) = (mod x 2 == 0) && (secuenciaPar (xs))


--An alternative answer with pattenr matching and guards combination
secuenciaPar' :: [Int] -> Bool
secuenciaPar' [] = True 
secuenciaPar' (x:xs)
 | ([x]==[]) = True
 | ((mod x 2) == 0) = secuenciaPar'(xs)
 | otherwise = False 

--Superpar: Make a function that determines whether all digits of a int number are even numbers

superpar :: Int -> Bool
superpar 0 = True
superpar n =  mod n 2 == 0 && superpar(div n 10) 

--Exercise 1.3 remake the div function manually
divides :: Int -> Int -> Bool
divides n d = rem d n == 0

--Exercise 1.3 a : Define the function ldf k n , that returns the number that is greater than k and the last divisor of n, use divides function

ldf :: Int -> Int -> Int 
ldf k n 
 | divides k n = k
 | k^2 > n = n 
 | otherwise = ldf (k+1) n 

--Another way to do prime list from 2 to m

prime0 :: Int -> Bool
prime0 n 
 | n<2 =False
 | otherwise = ldf 2 n == n
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
minChar ::[Char] -> Char
minChar [] = 'p'
minChar [x] = x
minChar (x:xs) = min x (minChar xs)

--Sorting Strings
srtString ::String -> String
srtString [] = []
srtString xs= m : (srtString (removeFirst (m) (xs))) where m = minChar xs

--Example 1.16 : Suppose we want to check wheter a string srt1 is a prefix of a str2. Then the answer to the question should be true or false i.e. the type declaration for prefix should run: prefix :: String -> String -> Bool.
{-
Prefix of a string ys are defined as follows:
[] is a prefix of ys 
if xs is a prefix of ys , then x:xs is a prefix of x:ys 
nothing else is a prefix of ys.
-}

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = x==y && prefix xs ys

prefix' :: String -> String -> Bool
prefix' [] _ = True
prefix' (x:xs) (y:ys) = x == y && prefix' xs ys

--1.17 Write a function  substring :: String -> String -> Bool that checks wheter str1 is a substring of str2 
{-The substring  of an arbitrary string ys  are given by :
1. if xs is a prefix of ys , xs is a substring
2. if ys is equals y:ys' and xs is a substring of ys', xs is a subtring of ys 
3. nothing else is a substring of ys 
-}
substring :: String -> String -> Bool 
substring [] ys = True
substring xs [] = False
substring xs (y:ys) = xs == (y:ys) || prefix xs (y:ys) || substring xs ys

