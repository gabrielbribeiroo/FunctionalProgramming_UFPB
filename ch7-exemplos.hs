-- Exemplos do Capítulo 7

import Data.Char
import Data.List

add :: Int -> Int -> Int 
add x y = x + y

add' :: Int -> (Int -> Int) 
add' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- twice (*2) 3
-- twice reverse [1,2,3,4,5]


map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b] 
map'' f []     = []
map'' f (x:xs) = f x : map'' f xs

-- map even [1,2,3,4]
-- map reverse ["abc","def","ghi"]
-- map (map (+1)) [[1,2,3],[4,5]]


filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x:xs) | p x       = x : filter'' p xs
                  | otherwise = filter'' p xs

-- filter even [1..10]
-- filter (> 5) [1..10]
-- filter (/= ' ') "abc def ghi"

sumsqreven :: [Int] -> Int 
sumsqreven ns = sum (map (^2) (filter even ns))


summ :: Num a => [a] -> a
summ []     = 0
summ (x:xs) = x + summ xs

prod :: Num a => [a] -> a
prod []     = 1
prod (x:xs) = x * prod xs

andd :: [Bool] -> Bool
andd []     = True
andd (x:xs) = x && andd xs


foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f v []     = v
foldrr f v (x:xs) = f x (foldrr f v xs)

sumf :: Num a => [a] -> a
sumf = foldrr (+) 0

prodf :: Num a => [a] -> a
prodf = foldrr (*) 1

orf :: [Bool] -> Bool
orf = foldrr (||) False 

andf :: [Bool] -> Bool
andf = foldrr (&&) True

len :: [a] -> Integer
len = foldrr (\_ -> (\n -> 1+n)) 0


-- length [1,2,3]
-- length 1:[2,3]

-- = foldr (\x n -> 1 + n) 0 1:[2,3]
-- = (\x n -> 1 + n) 1 (fold (\x n -> 1 + n) 0 2:[3])
-- = (\x n -> 1 + n) 1 ((\x n -> 1 + n) 2 (fold (\x n -> 1 + n) 0 3:[]))
-- = (\x n -> 1 + n) 1 ((\x n -> 1 + n) 2 ((\x n -> 1 + n) 3 (fold (\x n -> 1 + n) 0 [])))
-- = (\x n -> 1 + n) 1 ((\x n -> 1 + n) 2 ((\x n -> 1 + n) 3 0)
-- = (\x n -> 1 + n) 1 ((\x n -> 1 + n) 2 ((\n -> 1 + n) 0)
-- = (\x n -> 1 + n) 1 ((\x n -> 1 + n) 2 (1 + 0)
-- = (\x n -> 1 + n) 1 ((\x n -> 1 + n) 2 1
-- = (\x n -> 1 + n) 1 ((\n -> 1 + n) 1
-- = (\x n -> 1 + n) 1 (1 + 1)
-- = (\x n -> 1 + n) 1 2
-- = (\n -> 1 + n) 2
-- = (1 + 2) 
-- = 3


revert :: [a] -> [a]
revert = foldrr (\x -> \xs -> xs ++ [x]) []

-- reverse [1,2,3]
-- reverse 1:[2,3]

-- = fold (\x xs -> xs ++ [x]) [] 1:[2,3]
-- = (\x xs -> xs ++ [x]) 1 (fold (\x xs -> xs ++ [x]) [] 2:[3])
-- = (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 (fold (\x xs -> xs ++ [x]) [] 3:[]))
-- = (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 ((\x xs -> xs ++ [x]) 3 (fold (\x xs -> xs ++ [x]) [] [])))
-- = (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 ((\x xs -> xs ++ [x]) 3 [])
-- = (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 ((\xs -> xs ++ [3]) [])
-- = (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 ([] ++ [3])
-- = (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 [3])
-- = (\x xs -> xs ++ [x]) 1 ((\xs -> xs ++ [2]) [3])
-- = (\x xs -> xs ++ [x]) 1 ([3] ++ [2])
-- = (\x xs -> xs ++ [x]) 1 [3,2]
-- = (\xs -> xs ++ [1]) [3,2]
-- = ([3,2] ++ [1])
-- = [3,2,1]

------ Revisar Exemplo do slide
-- append :: [a] -> [a] -> [a]
-- append ys = foldr (:) ys
append :: [a] -> [a] -> [a]
append xs ys = foldrr (:) ys xs


-- [1,2,3] ++ [4,5,6]

-- = fold (:)  [4,5,6]  [1,2,3]
-- = fold (:)  [4,5,6]  1:[2,3]
-- = 1:(fold (:) [4,5,6] 2:[3])
-- =  1:(2:(fold (:) [4,5,6]  3:[]))
-- =  1:(2:(3:(fold (:) [4,5,6]  [])))
-- =  1:(2:(3:([4,5,6])))
-- =  1:(2:(3:[4,5,6]))
-- =  1:(2:[3,4,5,6])
-- =  1:[2,3,4,5,6]
-- =  [1,2,3,4,5,6]



(@) :: (b -> c) -> (a -> b) -> (a -> c)
f @ g = \x -> f(g x)

odd' = not . even

twice' f = f . f

sumsqreven' = sum . map (^2) . filter even



oddd :: Int -> Bool
oddd = not @ even

alll :: (a -> Bool) -> [a] -> Bool
alll p xs = and [p x | x <- xs]

anyy :: (a -> Bool) -> [a] -> Bool
anyy p xs = or [p x | x <- xs]

tkWhile :: (a -> Bool) -> [a] -> [a]
tkWhile p [] = []
tkWhile p (x:xs)
   | p x       = x : tkWhile p xs
   | otherwise = []
   
dpWhile :: (a -> Bool) -> [a] -> [a]
dpWhile p [] = []
dpWhile p (x:xs)
   | p x       = dropWhile p xs
   | otherwise = x:xs

-----------------------------------
-- Binary String Transmiter
-----------------------------------

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits] 
                 where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int 
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit] 
int2bin 0 = [] 
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit] 
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]] 
chop8 []   = [] 
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit] 
channel = id

transmit :: String -> String 
transmit = decode . channel . encode

-----------------------------------
-- Voting algorithms
-----------------------------------

votes :: [String] 
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int 
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a] 
rmdups [] = [] 
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)] 
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a 
winner = snd . last . result

-- Alternative voting system

ballots ::   [[String]] 
ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]


rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]] 
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a] 
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a 
winner' bs = case rank (rmempty bs) of 
               [c]   -> c 
               (c:_) -> winner' (elim c bs)


-- exercises
all :: (a -> Bool) -> [a] -> Bool 
all _ [] = True 
all p (x:xs) = p x && all p xs

any :: (a -> Bool) -> [a] -> Bool 
any _ [] = False 
any p (x:xs) = p x || any p xs 

takeWhile :: (a -> Bool) -> [a] -> [a] 
takeWhile _ [] = [] 
takeWhile p (x:xs) 
   | p x = x : takeWhile p xs 
   | otherwise = [] 

dropWhile :: (a -> Bool) -> [a] -> [a] 
dropWhile _ [] = [] 
dropWhile p (x:xs) 
   | p x = dropWhile p xs 
   | otherwise = x:xs 

foldr :: (a -> b -> b) -> b -> [a] -> b
map :: (a -> b) -> [a] -> [b] 
map f = foldr (\x acc -> f x : acc) [] 

filter :: (a -> Bool) -> [a] -> [a] 
filter p = foldr (\x acc -> if p x then x : acc else acc) [] 

dec2int :: [Int] -> Int 
dec2int = foldl (\acc x -> acc * 10 + x) 0