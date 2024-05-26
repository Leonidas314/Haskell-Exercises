imp :: Bool -> Bool -> Bool 
imp True False = False
imp _ _ = True

data Nat = Zero | Succ Nat


uno = Succ Zero
dos = Succ uno
tres = Succ tres
cuatro = Succ tres 

sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat Zero n = n 
sumaNat m Zero = m 
sumaNat (Succ n) m = Succ(sumaNat n m)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = sumaNat m (mult n m)

par :: Nat -> Bool 
par Zero = True
par (Succ Zero) = False
par n = par (n - Succ(Succ Zero))
instance Show Nat where 
    show Zero = "Zero"
    show (Succ(n))="Succ("++ show n++")"

instance Num Nat where
    (-) Zero Zero = Zero
    (-) Zero n = error "Neg"
    (-) n Zero = n
    (-) (Succ m) (Succ n) = (-) m n 

instance Eq Nat where
    (==) n m = n == m

instance Ord Nat where
    (<) Zero Zero = False
    (<) Zero _ = True
    (<) _ Zero = False
    (<) (Succ n) (Succ m) = (<) n m
    (>) Zero Zero = False
    (>) Zero _ = False
    (>) _ Zero = True
    (>) (Succ m) (Succ n) = (>) m n
    (<=) Zero Zero = True
    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ m) (Succ n) = (<=) m n
count :: [Int] -> Int -> Int
count [] _ = 0
count (x:xs) y = if x == y then 1 + (count xs y) else (count xs y)

countG :: [Int] -> Int -> Int
countG [] _ = 0
countG (x:xs) y 
 | y==x = 1 + (countG xs y)
 | otherwise = countG xs y