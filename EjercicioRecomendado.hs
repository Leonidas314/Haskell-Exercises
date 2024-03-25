--Agarrar una secuencia y decir si todos sus digitos son pares
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

selectionSort :: [a] -> [a]
selectionSort [] = []
selectionSort [x] = []
selectionSort (x:xs) == [x | x <- , ]
selectionSort (x,y:xs)
 | y<x = y
 | 
