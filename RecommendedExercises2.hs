--For the calculator's project: I need a function that recibe a String with a mathematical expression and returns the normal expresion on Float type

import Data.Char (ord , isDigit)
import Data.Ratio

--Function that convert the first numerical value into a list with the digits 
listFirstFloat :: String -> [Float]
listFirstFloat [] = []
listFirstFloat (x:xs) 
 | isDigit x = (fromIntegral(ord x) - 48) : listFirstFloat xs
 | otherwise = listFirstFloat [] --Ready to go!
 
--Function that convert a list of numbers in a integer

listToFloat :: [Float] -> Float
listToFloat [x] = x
listToFloat xs = last xs + (10 * (listToFloat (init (xs)))) --Ready to go!

--Now I need a main function that take a string and diference if the next operation is a multiplication or a division...

--Can i do a "removefirstFloat" function with the same logic that  listFirstFloat?

removefirstFloat :: String -> String
removefirstFloat [] = []
removefirstFloat(x:xs)
 | isDigit x = removefirstFloat (xs)
 | otherwise = (x:xs)  --Ready to go!

removeOperator :: String -> String
removeOperator [] = []
removeOperator (x:xs)
 | (x == '*') || (x == '/') = xs
 |otherwise = []
--back to the main function ...



--Now I need a main function that take a string and diference if the next operation is a multiplication or a division...
--How i know if the next char is a operator "*" or "/"?

reduction :: String -> Float
reduction [] = error "Invalid expression"
reduction (x:xs)  
 |isDigit x && removefirstFloat (x:xs) == "" = listToFloat(listFirstFloat(x:xs))
 |isDigit x && (head(removefirstFloat (x:xs))=='*')= listToFloat(listFirstFloat(x:xs)) * reduction(removeOperator(removefirstFloat(x:xs)))
 |isDigit x && (head(removefirstFloat (x:xs))=='/')= listToFloat(listFirstFloat(x:xs)) / reduction(removeOperator(removefirstFloat(x:xs)))
 |otherwise = reduction[]  
