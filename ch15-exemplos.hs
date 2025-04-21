-- Exemplos do Capítulo 15

infinity = 1 + infinity

ones = 1 : ones

replic :: Int -> a -> [a]
replic 0 _ = []
replic n x = x : replicate (n-1) x

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) =
    p : sieve [x | x <- xs, mod x p /= 0]

twin :: (Int,Int) -> Bool
twin (x,y) = y == x+2

twins :: [(Int,Int)]
twins = filter twin (zip primes (tail primes))

fibs :: [Int]
fibs = add [0,1..]

add :: [Int] -> [Int]
add (x:(y:xs)) = x:add(y:(x+y):xs)  