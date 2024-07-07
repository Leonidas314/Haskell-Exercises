
--Funcion que determina si en una lista de enteros la suma de los numeros impares es igual a la de los numeros pares
f :: [Int] -> Bool
f [] = True
f (x:xs)= aux 0 (x:xs) == aux2 0 (x:xs) where aux n [] = n
                                              aux n (x:xs) = if mod x 2 == 0 then aux (n+x) xs else aux n xs
                                              aux2 n [] = n
                                              aux2 n (x:xs) = if mod x 2 == 1 then aux2 (n+x) xs else aux2 n xs