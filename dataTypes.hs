data Nat = Zero | Succ Nat 

sumNat :: Nat -> Nat -> Nat
sumNat Zero m = m
sumNat n Zero = n 
sumNat (Succ n) (Succ m) = Succ(sumNat n (Succ m))

multNat :: Nat -> Nat -> Nat 
multNat Zero _ = Zero
multNat _ Zero = Zero
multNat (Succ Zero) n = n
multNat m (Succ Zero) = m
multNat (Succ m) (Succ n)= multNat m (sumNat (Succ n) (Succ n))

par :: Nat -> Bool 
par Zero = True
par (Succ Zero) = False
par (Succ(Succ m)) = par m 

impar :: Nat -> Bool
impar Zero = False
impar (Succ Zero) = True
impar (Succ(Succ Zero))= False
impar (Succ(Succ(Succ(m))))= impar (Succ m)

mult3 :: Nat -> Bool
mult3 Zero = True
mult3 (Succ(Succ(Succ(Zero))))= True
mult3 (Succ(Succ(Succ(m))))= mult3 m 
mult3 _ = False

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n =Succ(intToNat(n-1))

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n 

instance Eq Nat where
    (==) Zero Zero = True
    (==) (Succ m) (Succ n ) = m == n 
    (==) _ _ = False
    --Not need to define unequal (/=), this will work just defining (==)
instance Ord Nat where 
    (<=) Zero Zero = True 
    (<=) Zero (Succ Zero)= True
    (<=) _  Zero = False 
    (<=) (Succ n) (Succ m) = n <= m
    (>) Zero Zero = False
    (>) _ Zero = True 
    (>) Zero _ = False
    (>) (Succ n) (Succ m) = not (n <= m)
    --With this way is not need to define (>=)

instance Show Nat where 
    show Zero = "Zero"
    show (Succ n) = "Succ("++show n++")"


data Tree a = Empty | Node a (Tree a) (Tree a)
arbol0 = Node 1 Empty Empty
arbol1 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty) 
arbol2 = Node 1 (Node 2 (Node 3 (Node 4 (Node 5 Empty Empty) Empty) Empty) Empty) Empty

size :: Tree a -> Int
size Empty = 0
size (Node _ (leftTree ) (rightTree )) = 1 + size (leftTree ) + size (rightTree )

height :: Tree a-> Int
height Empty = 0
height (Node _ leftTree rightTree) = (1+ height leftTree) `max` (1+ height rightTree)

sumTree ::(Num a) => (Tree a) -> a
sumTree Empty = 0
sumTree (Node n left right) = n + (sumTree left) + (sumTree right)

sumleaves :: (Num a) => (Tree a) -> a 
sumleaves Empty= 0
sumleaves (Node n Empty Empty)= n
sumleaves (Node _ left right)= (sumleaves left) + (sumleaves right)

sumImpTree :: (Integral a) => (Eq a) => (Tree a) -> a
sumImpTree Empty = 0
sumImpTree (Node a left right) = if a`mod`2 /= 0 then a + ( sumImpTree left )+(sumImpTree right) else
    (sumImpTree left + sumImpTree right)

rev :: (Tree a) -> (Tree a)
rev Empty = Empty
rev (Node a left right) = (Node a (rev right) (rev left))

instance (Eq a) => Eq (Tree a) where 
    (==) Empty Empty = True
    (==) Empty _ = False
    (==) _ Empty = False
    (==) (Node a letfA rightA) (Node b leftB rightB) = a == b && letfA==leftB && rightA==rightB 

instance (Ord a) => Ord (Tree a) where
    (<=) Empty Empty = True
    (<=) Empty _ = True
    (<=) _ Empty = False

    (<=) (Node _ Empty Empty) (Node _ Empty Empty) = True

    (<=) (Node _ _ Empty) (Node _ Empty Empty) = False
    (<=) (Node _ Empty _) (Node _ Empty Empty) = False
    (<=) (Node _ _ _) (Node _ Empty Empty) = False

    (<=) (Node _ Empty Empty) (Node _ _ Empty) = True
    (<=) (Node _ Empty Empty) (Node _ Empty _) = True
    (<=) (Node _ Empty Empty) (Node _ _ _) = True

    (<) (Node a lA rA) (Node b lB rB) = (size (Node a lA rA)) < (size (Node b lB rB))
    (>) (Node a lA rA) (Node b lB rB) =not ((size (Node a lA rA)) < (size (Node b lB rB)))
    (>=) (Node a lA rA) (Node b lB rB) =not ((size (Node a lA rA)) < (size (Node b lB rB))) || (size (Node a lA rA)) == (size (Node b lB rB))

instance (Show a) => Show (Tree a) where
    show Empty = " _"
    show (Node a left right) = "Node("++" "++ show a ++" "++ show left ++" "++ show right ++ ")"

data (List a) = Null | Cons a (List a)

listA = Cons "a" (Cons "b" (Cons "c" Null))
listB = Cons "d" (Cons "e" Null)

instance (Show a ) => Show (List a) where
    show Null = ""
    show (Cons a lista) = "["++showContent (Cons a lista)++ "]" where 
        showContent Null = ""
        showContent (Cons a Null) = show a
        showContent (Cons a lista) = show a ++ "," ++ showContent lista 

instance (Eq a) => Eq (List a) where
    (==) Null Null = True
    (==) _ Null = False
    (==) Null _ = False
    (==) (Cons a listA) (Cons b listB) = a == b && listA == listB

last' :: (List a) -> a 
last' Null = error "Lista vacia"
last' (Cons a Null) = a 
last' (Cons a listA) = last' listA

head' :: (List a) -> a 
head' Null = error "Lista Vacia"
head' (Cons a _) = a 

tail' :: (List a) -> (List a)
tail' Null = error "Lista Vacia" 
tail' (Cons a Null) = Null
tail' (Cons a list) = list 

long :: (List a) -> Int
long Null = 0
long (Cons a list) = 1 + (long list)

concat' :: (List a) -> (List a) -> (List a)
concat' Null Null = Null
concat' Null (Cons a list)= (Cons a list)
concat' (Cons a list) Null = (Cons a list)
concat' (Cons a listA) (Cons b listB)= Cons a (concat' listA (Cons b listB))