

--Programas calculados por derivacion
----Ejercicio18 Definir la funcion 
--todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
--tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos de ys 
--todosOcurrenEn [1,5,2,5] [5,1,2,4] = True
--todosOcurrenEn [1,5,2,5] [5,2,4] = False

existElem :: Eq a => a -> [a] -> Bool
existElem _ []= False
existElem x (y:ys) = (x==y) || existElem x ys

toEn :: Eq a => [a] -> [a] -> Bool
toEn [] _ = True 
toEn (x:xs) (ys) = existElem x ys && toEn xs ys 

--Definir prefijo

prefijo :: String -> String -> Bool
prefijo [] _ = True
prefijo (x:xs) (y:ys) = x == y && prefijo xs ys