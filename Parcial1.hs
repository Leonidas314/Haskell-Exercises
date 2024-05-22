xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True
--lista en Haskell
data Lista a = Null | Cons a (Lista a)
instance (Show a) => Show (Lista a) where 
    show lista= aux 0 lista  
        where
            aux 0 lista = "[" ++ aux 1 lista
            aux _ Null = "]"
            aux _ (Cons a Null) = show a ++ "]"
            aux _ (Cons a (next)) = show a ++","++ aux 1 (next)


instance (Eq a) => Eq (Lista a) where
    Null == Null = True
    Cons a (nexta) == Cons b (nextb) = a==b && nexta == nextb

head' :: Lista a -> a
head' Null = error "Lista Vacia"
head' (Cons a (next)) = a

last' :: Lista a -> a
last' Null = error "Lista Vacia"
last' (Cons a (Null)) = a
last' (Cons a (next)) = last' (next)

tail' :: Lista a -> Lista a 
tail' Null = error " Lista Vacai"
tail' (Cons a (next)) = next

a = Cons 1 (Cons 2 (Cons 3 Null))

concatenar :: Lista a -> Lista a -> Lista a 
concatenar Null Null = Null
concatenar list Null = list
concatenar Null list = list
concatenar (Cons a nextA) listb = Cons a (concatenar nextA listb)

enRango :: Int -> Int -> [Int] -> [Int]
enRango a b xs = [x | x<- xs , x>=a, x <=b]

falses :: [Bool]
falses = False:falses

