--Guía 1 de ejercicios curso Programación avanzada

--Ejercicio 1: Factorial de un numero recursiva
factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n-1)
--Ejercicio 2 : Igualdad de expresiones
isEqual :: Float -> Float -> Bool
isEqual x y = x == y 
--Ejercicio 3 
secondChar :: String -> Char 
secondChar s =head (tail s)
--Ejercicio 4
lastChar :: String -> Char
lastChar s = head (reverse s)
--Ejercicio 5 Es par?
mult2 :: [Int] -> Bool
mult2 l 
 | mod (head(reverse(l))) 2 == 0 = True
 | otherwise = False
--Ejercicio 6 Es multiplo de 3 ?

mult3 :: [Int] -> Bool
mult3 l = mod (sum l) 3 == 0
--Ejercicio 7 Es multiplo de 6? Usar funciones anteriores

mult6 :: ([Int] -> Bool) -> ([Int] -> Bool) -> [Int] -> Bool
mult6 f1 f2 l = f1 l == f2 l


--Funcion reverse alternativa por Alvaro
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x] 

--Ejercicio 8 dado un numero, devolver una lista con sus cifras

intAlist :: Int -> [Int]
intAlist n
 | div n 10 > 0 = (intAlist (div n 10)) ++ [(mod n 10)]
 | div n 10 == 0 = [] ++ [mod n 10]


--Ejercicio 9 f1 == drop, f2 == take
cortar ::Int -> Int -> [a] -> (Int -> [a] -> [a]) -> (Int -> [a] -> [a]) -> [a]
cortar i j w f1 f2 = f2 (j-i) (f1 i w) 

cortar' :: Int -> Int -> [a] -> [a]
cortar' i j w = drop i (take j w) 

-- Main.hs

main :: IO ()
main = do
 print "Hola Mundo"