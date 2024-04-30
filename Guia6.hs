--Ejercio 1: Definir el tipo Nat visto en el teorico

data Nat = Zero|Succ Nat

--Instancias para las clases del tipo Nat(Ejercicio 5)

instance Show Nat where 
    show Zero = "Zero"
    show (Succ n) = "Succ("++show n++")" 
    --show = show.natToInt (muestra el Int)
    --Ver teorico y concatenar strings para mostrar
--Instancia de Eq Nat
instance Eq Nat where
    n == m = natToInt(n) == natToInt(m)
instance Num Nat where
--Instancia de la operacion Suma (+)
    (+) n Zero = n
    (+) n (Succ m)= Succ (n+m)
--Instancia de la operacion resta (-)
    (-) n Zero = n
    (-) Zero n = error "Not negatives values in Nat type"
    (-) (Succ n) (Succ m) = (-) n m
--Instacia de la operacion (*)
    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ n) (Succ m)
     | n== Zero = Succ m
     | otherwise= Succ m + (n * Succ m)


instance Ord Nat where
    --(<) (Succ n) Zero = False
    (<=) (Succ n) Zero  = False
    (<=) Zero (Succ n) = True
    (<=) Zero Zero = True
    (<=) (Succ n) (Succ m) = (<=) n m

--Ejercicio 2: Definir la funcion natToInt : Nat → Int que dado un numero Nat retorna su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2.

natToInt :: Nat -> Int 
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

--Ejercicio 3. Definir la funcion intToNat : Int → Nat que dado un numero entero retorna su Nat correspondiente. Por ejemplo: intToNat 2 = (Suc(Suc Zero)).

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ(intToNat (n-1))

--Ejercio 4 Definir sumaNat :: Nat -> Nat -> Nat
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero (Succ n) = Succ n
sumaNat (Succ n) Zero = Succ n
sumaNat Zero Zero = Zero
sumaNat (Succ n) (Succ m) = (Succ n) +(Succ m)

--Ejercicio5 : Instanciar las Clases Eq, Ord, Show para Nat sin utilizar deriving. Averiguar que orden define Haskell si se utiliza deriving Ord para Nat

--Ejercicio 6: Definir Arboles Binarios --Ejercicio 9 Instanciar Eq, Ord Show Para arboles

data BinTree a = Null | Node (BinTree a) (BinTree a) a 

instance (Eq a) => Eq (BinTree a) where 
    (==) Null Null = True
    (==) Null (Node _ _ _) = False
    (==) (Node hi hd a) (Node hi2 hd2 b) = (a == b) && (hi==hi2) && (hd == hd2) 

instance (Show a) => Show (BinTree a) where
    show Null = "[]"
    show (Node hi hd a) = "hi"++show hi++ show a ++ "hd" ++ show hd


--Ejercicio 7 : Funcion size que retorna la cantidad de nodos de un arbol
size :: BinTree a -> Int
size Null  = 0
size (Node hi hd _)= 1 + size hi + size hd

--Ejercicio 8 : Funcion height que devuelve la altura de un arbol

height :: BinTree a -> Int 
height Null = 0
height (Node hi hd _) = 1 + max (height hi) (height hd) 
--Sugerido: Definir factorial con Nat
{-}
facNat :: Nat -> Int
facNat Zero = nat-}