--The Prime Factorization Algorithm
--To make it we need the next function

--last prime divisor lpd k n: retunrs the last prime divisor between k and n
lpd :: Int -> Int -> Int 
lpd k n
 | n <= 1 = error "Not a prime number"
 | mod n k == 0 = k
 | k^2 > n = n
 | otherwise = lpd (k+1) n

--Algorithm that take an integer and returns a list with factorization

factorization :: Int -> [Int]
factorization 1 = []
factorization n = lpd 2 n : factorization(n`div`(lpd 2 n))

--Map And Filter

--Exercise 1.20 Use map to write a function that take a list of list and returns a list of the corresponding list length

listOflengthList :: [[a]] -> [Int]
listOflengthList l = [x | x <- map length l]


--Exercise 1.21 Use map to write a function sumLengths that take a list of list and return the sum of their lengths

sumLengths :: [[a]] -> Int 
sumLengths [] = 0
sumLengths (x:xs) = length x + sumLengths xs