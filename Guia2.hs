--Ejercicio2: Definir las funciones Head Tail last Init

--head retorna el primer elemento de una lista
head' :: [a] -> a
head' (x:xs) = x 
--tail retorna toda la lista menos el primer elemento
tail' :: [a] -> [a]
tail' (x:xs) = xs

--last' retorna el ultimo elemento
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

--init' retorna todos los elementos de la lista menos el ultimo
init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = [x] ++ init' xs

--Ejercicio 3 obtener el maximo de tres interos

maxTres :: Int -> Int -> Int -> Int
maxTres x y z 
 | x >= y && x >= z = x
 | y >= z && y >= x = y 
 | otherwise = z

--Ejercicio 4 definir las funciones concatenar, tomar , dejar y agregar al final
concatenar :: [a] -> [a] -> [a]
concatenar a b = a ++ b

tomar :: Int -> [a] -> [a]
tomar 0 (x:xs) = []
tomar n (x:xs) = [x] ++ (tomar (n-1) (xs))

dejar :: Int -> [a] -> [a]
dejar 0 (x:xs) = [x] ++ xs
dejar n (x:xs) = [] ++ dejar (n-1) (xs)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal lista elemento  = lista ++ [elemento] 

--Ejercicio 5 definir funcion abs 

absolute :: Int -> Int
absolute x 
 | x>=0 = x 
 |otherwise = -x

--Ejercicio 6 (Pendiente)
edad :: (Num a) => (a,a,a) -> (a,a,a) -> a
edad (d1,m1,a1) (d2,m2,a2)  = a1-a2

--Ejercicio 7 : Definir  la funcion xor : disyunción exclusiva

xor :: Bool -> Bool -> Bool 
xor False x = x
xor x False = x 
xor True True = False

--Ejemplo De funcion de conjuncion logica
conjuncion :: Bool -> Bool -> Bool
conjuncion False x = False  
conjuncion True x = x 

--Funcion biImplicación Ejemplo:
biImplication :: Bool -> Bool -> Bool
biImplication p q = p==q

--Ejercicio8 Defina una fucion que dado un numero natural defina si es primo o no
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [2..n] , mod n x == 0] == [n] 

{-Ejercicio9 Defina una función que dado un número naturale n, retorne la lista de todos los primos 
 naturales-}
listaDePrimos :: Int -> [Int]
listaDePrimos n =[x | x <- [2..n] , esPrimo x]

--Ejercicio 10 Defina una funcion que dada una lista retorne la inversa de la misma por Leon
invertirLista :: [a] -> [a]
invertirLista [] = []
invertirLista (x:xs) = invertirLista xs ++ [x]

--Ejercicio 11  defina una funcion que reciba una lista de numeros y devuelva solo los numeros primos
filtrarPrimos :: [Int] -> [Int]
filtrarPrimos [] = []
filtrarPrimos (x:xs) = [x | esPrimo x] ++ filtrarPrimos xs   

--Ejercicio 12 definir una funcion que determine si una lista es palindromo

palindromos :: String -> Bool
palindromos [] = True 
palindromos [x] = True
palindromos (x:xs) = x == last xs && palindromos (init xs) 

--Ejercicio13 defina una funcion que dados los coheficientes de una ecuacion cuadratica xa²+bx+c = 0
--devuelva la cantidad de raices reales de la ecuacion

realRoot :: Int -> Int -> Int -> Int 
realRoot a b c 
 | b*b - 4 * a * c >= 0 = 2
 | b*b - 4 * a * c < 0 = 0
