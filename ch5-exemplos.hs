-- Exemplos do Capítulo 5

concatena :: [[a]] -> [a]
concatena xss = [x | xs <- xss, x <- xs]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


prime :: Int -> Bool
prime n = factors n == [1,n]


primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]


pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)


sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]


positions :: Eq a => a -> [a] -> [Int]
positions x xs =
   [i | (x',i) <- zip xs [0..], x == x']


count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']


--permutações

permut :: Eq a => [a] -> [[a]]
permut [] = [[]]
permut (x:xs) = [a:p | a <- (x:xs), p <- permut xs]



-- exercicios
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]


perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(fatores x) == x]
   where fatores num = [i | i <- [1..num-1], num `mod` i == 0]


cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | y <- ys | x <- xs]


positions :: Eq a => a -> [a] -> [Int] 
positions x xs = [i | (y, i) <- zip xs [0..], Just y == find (== x) [y]] 