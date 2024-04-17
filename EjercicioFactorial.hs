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

--Primera solucion
--Factorial con una definicion local de aux
factoriales_1 :: Int -> [Int]
factoriales_1 n =
    reverse (aux n) where aux 0 = [1] 
                          aux n = factorial n : aux(n-1)

--Segunda solucion :
--Definicion recursiva con uso de acumladores --Actividad: Completar
factoriales_2 :: Int -> [Int]
factoriales_2 n =
    reverse(aux(n+1) 0 [1]) --reverse esta demás con la siguiente def.
    where aux n m (x:xs) = if m == n 
            then xs else
                          aux n (m+1) (factorial (m+1):(x:xs))
                             --else error "NaN"
                             --No mentira geniooooo

--Tercera solucion: Por listas de comprensión
factoriales_3 :: Int -> [Int]
factoriales_3 n = [factorial x | x <- [0..n]]

--Cuarta Solucion Definicion de orden superior
factoriales_4 :: Int -> [Int]
factoriales_4 n = map factorial [0..n]

--Actividad proponer una nueva solucion con la funcion scanl
{-scanl toma una funcion binaria, una semilla y una lista y devuelve los resultados
acumulados de aplicar la funcion a la semilla y a los valores sucesivos-}

-- Atencion! Las funciones definidas previamente NO son binarias 

factoriales_5 :: Int -> [Int]
factoriales_5 n = scanl (*) 1 [1..n]