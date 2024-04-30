--definir la funcion deleteN :: Eq a => Int -> a -> [a] -> [a]
--tal forma que delete n x xs es la lista obtenida de eliminar las primras n courrencias de x  

deleteN :: Eq a => Int -> a -> [a] -> [a]
deleteN 0 _ xs =  xs
deleteN _ _ [] = []
deleteN n y (x:xs) 
 |x==y = deleteN (n-1) y xs
 |otherwise = [x] ++ deleteN n y xs

--Definir la funcion allOcurrIn :: Eq=>a -> [a] -> [a] -> Bool con listas de comprension

allCurrIn:: Eq a => [a] -> [a] -> Bool
allCurrIn (x:xs) (y:ys) = [n | n <- xs ,not(elem n ys)]  == []