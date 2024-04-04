--For the calculator's project: I need a function that recibe a String with a mathematical expression and returns the normal expresion on Int type
import Data.Char (ord , isDigit)
import Data.Ratio
--prod :: String -> Int 
--prod [] = 1
--prod (x:y:xs)

--Function that convert a String in a list of int if the char is a digit
listFirstInt :: String -> [Int]
listFirstInt [] = []
listFirstInt (x:xs) 
 | isDigit x = (ord x - 48) : listFirstInt xs
 | otherwise = listFirstInt [] --Ready to go!
 
--Function that convert a list of numbers in a integer

listToInt :: [Int] -> Int 
listToInt [x] = x
listToInt xs = last xs + (10 * (listToInt (init (xs)))) --Ready to go!

--Now I need a main function that take a string and diference if the next operation is a multiplication or a ...
{-
reduction :: String -> Int 
reduction [] = 1
reduction (x:xs)-}

--Can i do a "removefirstInt" function with the same logic that  listFirstInt?

removefirstInt :: String -> String
removefirstInt [] = []
removefirstInt(x:xs)
 | isDigit x = removefirstInt (xs)
 | otherwise = (x:xs)  --Ready to go!

removeOperator :: String -> String
removeOperator [] = []
removeOperator (x:xs)
 | (x == '*') || (x == '/') = xs
 |otherwise = []
--back to the main function ...

--How i know if the next char is a operator "*" or "/"?


reduction :: String -> Int 
reduction [] = error "NaN"
reduction (x:xs)  
 |isDigit x && removefirstInt (x:xs) == "" = listToInt(listFirstInt(x:xs))
 |isDigit x && (head(removefirstInt (x:xs))=='*')= listToInt(listFirstInt(x:xs)) * reduction(removeOperator(removefirstInt(x:xs)))
 |isDigit x && (head(removefirstInt (x:xs))=='/')= listToInt(listFirstInt(x:xs)) `div` reduction(removeOperator(removefirstInt(x:xs)))
 
 |otherwise = reduction[]  -- listToInt(listFirstInt (x:xs))
 
--Error Prelude head = empty list... el error ocurre en la comparacion del head de la lista con el operador "*", llega la lista vacia.
--Creo que lo puedo resolver con una funcion que remueva el operador "*" luego de la evaluacion del mismo.z