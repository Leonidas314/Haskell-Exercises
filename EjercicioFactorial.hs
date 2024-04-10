--Factorial con pattern matching
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
--Factorial con guards
factorialguards:: Int -> Int
factorialguards n
 | n == 0 || n==1 = 1
 | n >= 0 = n*factorialguards (n-1)
 | otherwise = error "non factorial on negative numbers"

--Distintas formas de crear una lista con los factoriales de n:


--Factorial con una definicion local de aux
factoriales_1 :: Int -> [Int]
factoriales_1 n =
    reverse (aux n) where aux 0 = [1] 
                          aux n = factorial n : aux(n-1)


--Definicion recursiva con uso de acumladores
factoriales_2 :: Int -> [Int]
factoriales_2 n =
    reverse(aux(n+1) 0 [1]) --reverse esta demás con la siguiente def.
    where aux n m (x:xs) = if m == n 
            then [] else
                            if m/= n then factorial m: aux n (m+1) (x:xs)
                             else error "Sos un FRACASADO"
                             --No mentira geniooooo
         