-Ejercicio 1 : Utilizando la definicion de naturales vista en la materia ... Cual era ? definir una funcion que determine si  un natural es multiplo de tres o no.

data Nat = Zero | Succ Nat

instance Show Nat where
    show Zero = "Zero"
    show (Succ n) = "Succ("++ show n ++ ")"

instance Num Nat where
    (-) Zero Zero = Zero
    (-) Zero (Succ n) = error "Negativos"
    (-) n Zero = n
    (-) (Succ n) (Succ m) = n - m

multTres :: Nat -> Bool
multTres Zero = True
multTres (Succ Zero)= False
multTres (Succ(Succ Zero))= False
multTres n = multTres (n-Succ(Succ(Succ Zero)))

--Ejercicio2 : Definir una funcion que devuelva la suma del mayor subsegmento ordenado de una lista de ints.


split2 :: [a] -> [([a],[a])]
split2 xs = [(take i xs, drop i xs)| i <- [0.. length xs]]

--Crear la funcion split3 que dada una lista devuelva todas las formas de partila en tres en una tri-upla

split3 :: Eq a => [a] -> [([a],[a],[a])]
split3 xs = [(as,bs,cs)| (as , ys) <- split2 xs, (bs,cs) <- split2 ys]
a :: [Int] 
a = [8,6,3,25,4,3,4,5,2,3,4,5,6,3,2,1]
b :: [Int]
b = [3,6,7,9]

ordenados :: [Int] -> Bool
ordenados [] = True
ordenados [x] = True
ordenados (x:y:xs) = x<= y && ordenados (y:xs)

listasOrd :: [Int] -> [[Int]]
listasOrd xs = [bs | (as,bs,cs)<-split3 (xs) , ordenados bs]

--Funcion que compara la longitud de una lista con el maximo
 --de las longitudes de un lista de listas 

comparacion :: [Int]-> [[Int]] ->Bool
comparacion xs ys= length xs == maximum (map length ys) 

sumMayOrd :: [Int] -> Int
sumMayOrd xs = sum ([as | as <- listasOrd xs , comparacion as (listasOrd xs)]!!0)

-- Alvi
sumMayOrd' :: [Int] -> Int
sumMayOrd' xs = sum $ maxLenght [bs | (as,bs,cs)<-split3      (xs) , ordenados bs]

maxLenght :: [[Int]] -> [Int]
maxLenght xs = aux [] xs
    where
        aux acc [] = acc
        aux acc (x:xs)
            | length acc < length x = aux x xs
            | length acc >= length x = aux acc xs
--Ejercicio3: Funcion plain dada una lista de listas regresar las listas en una sola por ejemplo [[1],[2],[3]] -> [1,2,3]

plain :: [[a]]->[a]
plain xs = foldr (++) [] xs


--Definir una funcion por listas de comprension que determiner si un elemento de la lista es igual a la suma de los elementos del resto de la lista.
a1::[Int]
b1::[Int]
a1 = [1,1,1]
b1=[1,2,3]

--Ejercicio 4:Funcion que me devuelve una lista de la sumatoria de los restos para cada i
f1 :: [Int]-> [Int]
f1 xs = [(sum xs) - xs !! i | i <- [0..(length xs - 1)] ]

f2 :: [Int]-> Bool
f2 xs = or ([any (x==) (f1 xs) |x<-xs])


--Alvi
f3 :: [Int] -> Bool
f3 xs = or [x == b-x | x<-xs , let b =sum xs] 

f4 :: [Int] -> Bool
f4 xs =or [ x == sum xs - x | x <- xs]

{-
falses = (False:falses)
Ejercicio d (Reduccion orden normal)
foldr (and) True falses == False 
    ={def ==} -> {def foldr} -> {def falses}
foldr (and) True (False:falses) == False
    ={def ==} -> {def foldr}
False `and` foldr (and) True falses == False
    ={def ==} -> {def `and`} => False `and` _ = False
False == False
={def ==}
True []

Ejercicio d (Reduccion Apli.)
foldr (and) True falses == False

(==) (foldr (and) True falses) False donde foldr es mi primer parametro y False el segundo

={def falses}

(==) (foldr (and) True (False:falses)) False

={def falses}

(==) (foldr) (and) True (False:(False:falses)) False 

}