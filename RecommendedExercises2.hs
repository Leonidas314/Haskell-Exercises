--For the calculator's project: I need a function that recibe a String with a mathematical expression and returns the normal expresion on Int type
Import Data.Char (ord , isDigit)

prod :: String -> Int 
prod [] = 1
prod (x:y:xs)