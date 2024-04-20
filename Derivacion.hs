
--Programas calculados por derivacion

----Ejercicio18 Definir la funcion 
--todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
--tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos de ys 
--todosOcurrenEn [1,5,2,5] [5,1,2,4] = True
--todosOcurrenEn [1,5,2,5] [5,2,4] = False

existElem :: Eq a => [a] -> a -> Bool
existElem [] _= False
existElem (y:ys) x = (x==y) || existElem ys x

toEn :: Eq a => [a] -> [a] -> Bool
toEn [] _ = True 
toEn (x:xs) (ys) = existElem ys x && toEn xs ys 


prefix' :: String -> String -> Bool
prefix' [] _ = True
prefix' (x:xs) (y:ys) = x == y && prefix' xs ys