
--Ejercicio 1:
--a)
sontodasIg :: Eq a => [a] -> Bool
sontodasIg [] = True    
sontodasIg [x] = True
sontodasIg (x:xs)= all (==x) (x:xs)
--b)
sontodosDif :: Eq a => [a] -> Bool
sontodosDif [] = True
sontodosDif [x] = True
sontodosDif (x:xs) = all (/=x) [y | y <-xs] && sontodosDif (xs)
--c)
sorted :: [Int] -> Bool
sorted [] = True
sorted (x:xs) = all (x<=) [y | y <- xs] && sorted xs

--d)
oneThenZero ::  [Int] -> Bool
oneThenZero []= True
oneThenZero (x:xs) = if x==1 then any (==0) xs else oneThenZero xs

--e)
esPrimo :: Int -> Bool
esPrimo n = [i | i <- [2..n] , n `mod` i == 0] == [n]

multPrimes :: [Int] -> Int
multPrimes xs = foldr (*) 1 [x | x<-xs , esPrimo x]

--Ejercicio2: Sea xs una lista no vacia con elementos boolesanos, tal que true aparezca al menos una vez enla lista. especificar:

--a) 

minposTrue :: [Bool] -> Int
minposTrue xs = minList [n | n<-[0..(length xs)-1] , xs!!n==True] where minList [x] = x
                                                                        minList (x:xs) = min x (minList xs) 
               
--b)
maxposTrue :: [Bool] -> Int
maxposTrue xs = maxList [n | n<-[0..(length xs)-1] , xs!!n==True] where maxList [x] = x
                                                                        maxList (x:xs) = max x (maxList xs)


--Ejercicio3 

--a)

sameOddEven :: [Int] -> Bool
sameOddEven xs = length [x | x <- xs , x`mod`2==0] == length[y | y <- xs , y /= 0 , y `mod`2 /= 0 ]
--b)

prefijo :: Eq a => [a] -> [a] -> Bool
prefijo xs ys = any (ys==) [take i xs | i <- [0..(length xs)]]

subsec1 :: Eq a => [a] -> [a] -> Bool
subsec1 _ [] = True
subsec1 [] ys = False
subsec1 (x:xs) ys = prefijo (x:xs) ys || subsec1 xs ys

--c)
endsec ::Eq a => [a] -> [a] -> Bool
endsec xs ys = any (==ys) [drop i xs | i <- [0..length xs]]


--Definir split2:
--La funcion split2 toma una lista de tipo a y devuelve una lisra de duplas con todas la forams de partir en dos a la lista ingresada.
--La funcion se define con una lista de comprension que define la dupla, el primer elemento de la dupla es la funcion take que toma los i elementos de xs empezando por el 0 y hasta #xs
--el segundo componente de la dupla se define con la funcion drop, que va descartando los i elementos de la lista empezando por 0 hasta #xs
split2 :: [a] -> [([a],[a])]
split2 xs = [(take i xs ,drop i xs) | i <- [0..length(xs)]]

--Definir split3 usando split2 
--Como funciona split3:
--Con una lsita de comprension definimos una 3-upla (as,bs,cs)
--usamos split2 para generar una lista de duplas con todas las formas de partir en dos a xs y las guardamos en (as,ys), de esta forma ya tenemos todas la particiones de as, e ys nos sirve para , al aplicar split2 ys obtenemos las componenetes bs y cs. ys sirve como una lista auxiliar.
split3 :: [a] -> [([a],[a],[a])]
split3 xs = [(as,bs,cs) | (as,ys)<-split2(xs) , (bs,cs)<-split2(ys)]

--Usando estas funciones resolver :

--Ejercicio4 
--a)
minsumsec :: [Int] -> Int
minsumsec xs =minList [sum bs | (as,bs,cs)<-split3 xs] where minList [x] = x
                                                             minList (x:xs) = min x (minList xs)
maxigual :: [Int] -> Int
maxigual xs = maxList [sum bs | (as,bs,cs)<-split3 xs ,sontodasIg bs] where maxList [x]= x
                                                                            maxList (x:xs) = max x (maxList xs)
maxdistinto :: [Int] -> Int
maxdistinto xs  = maxList [sum bs | (as,bs,cs) <- split3 xs, sontodosDif bs] where maxList [x]=x
                                                                                   maxList (x:xs) = max x (maxList xs)

--Ejercicio 5                                                                                   
--subsecuencia
subsec ::Eq a => [a]-> [a] -> Bool
subsec xs ys = any (==ys) [ bs | (as,bs,cs)<-split3 xs]
--Subsecuencia final
endingSubseg :: Eq a => [a]-> [a] -> Bool
endingSubseg xs ys = any (==ys) [ cs | (as,bs,cs)<-split3 xs]

--Subsegmento de suma maxima

maxsumsec :: [Int] -> Int
maxsumsec xs =maxList [sum bs | (as,bs,cs)<-split3 xs] where maxList [x] = x
                                                             maxList (x:xs) = max x (maxList xs)

split4 :: [a] -> [([a],[a],[a],[a])]
split4 xs = [(as,bs,cs,ds) | (as,ys)<-split2 xs, (bs,cs,ds)<-split3 ys]                                                             