data (Bintree a) = Null | Node (Bintree a) (Bintree a) a
arb :: Bintree Int
arb = Node(Node (Node Null Null 5) Null 9) (Node Null Null 4) 3 

f :: (Bintree Int) -> Int
f Null = 0
f (Node hi hd n)= n + f hi + f hd 

sumaseg :: [Int] -> Int
sumaseg []= 0
sumaseg [x] = 0
sumaseg (x:y:xs)= x+ sumaseg (y:xs)