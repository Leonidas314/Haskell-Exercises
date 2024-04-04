import Data.Char (ord , isDigit)
divRatio :: Float -> Float -> Float
divRatio x y = x / y 

--Function that convert a String in a list of int if the char is a digit
listFirstInt :: String -> [Float]
listFirstInt [] = []
listFirstInt (x:xs) 
 | isDigit x = (fromIntegral(ord x) - 48) : listFirstInt xs
 | otherwise = listFirstInt [] --Ready to go!

listToInt :: [Float] -> Float
listToInt [x] = x
listToInt xs = last xs + (10 * (listToInt (init (xs)))) --Ready to go!
