data (BinTree a) = Null | Cons(BinTree a) (BinTree a) a
data (Lista a)  = Nill | Consl (Lista a) a

arbol = Cons (Cons Null Null 3) (Cons Null Null 4) 5
lista = Consl (Consl (Consl (Consl Nill 7) 2) 3) 4  

sumOdd :: (Integral a) => (BinTree a) -> Int
sumOdd Null = 0
sumOdd (Cons m n x) = odd'(fromIntegral x) +  (sumOdd m) + (sumOdd n) where odd' x =  if odd x then x else 0

sumOddList :: (Num a) => (Lista a) -> a
sumOddList Nill = 0
sumOddList (Consl l x) = (x) + (sumOddList l)
                            where odd' x = if odd x then x
                                                else 0


lissta = [0..9]

parImpar :: [Int] -> Bool
parImpar xs = and [xs !! i `mod` 2 == 0 | i<- [0..length xs-1], i`mod`2 == 0] 
                    && and [xs !! j `mod` 2 == 1 | j<-[0..length xs-1], j`mod`2 ==1] 
                        && length xs `mod` 2 == 0                                   



sumarPrev :: [Int] -> Int -> Int 
sumarPrev xs i = sum [xs !! j | j <- [0..i-1] ]

f :: [Int] -> Bool
f [] = False
f [x] = False
f xs = and [xs !! i == sumarPrev xs i | i <-[1..length xs-1] ]

