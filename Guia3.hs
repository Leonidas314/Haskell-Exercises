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
 | (y:ys)== [] = merge (x:xs) []
