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
