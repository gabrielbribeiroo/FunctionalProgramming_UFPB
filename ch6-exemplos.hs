-- Exemplos do Capítulo 6

fac :: Integer -> Integer
fac n = product [1..n]

fat :: Integer -> Integer
fat 0 = 1
fat n = n * fat(n - 1)

produto :: Num a => [a] -> a
produto []     = 1
produto (n:ns) = n * produto ns

lengthh :: [a] -> Int
lengthh []     = 0
lengthh (_:xs) = 1 + lengthh xs

inverse :: [a] -> [a]
inverse []     = []
inverse (x:xs) = inverse xs ++ [x]

zipp :: [a] -> [b] -> [(a,b)]
zipp []     _      = []
zipp _      []     = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

dropp :: Int -> [a] -> [a]
dropp 0 xs     = xs
dropp _ []     = []
dropp n (_:xs) = dropp (n-1) xs

(+++) :: [a] -> [a] -> [a]
[]     +++ ys = ys
(x:xs) +++ ys = x : (xs ++ ys)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
   qsort smaller ++ [x] ++ qsort larger
   where
      smaller = [a | a <- xs, a <= x]
      larger  = [b | b <- xs, b > x]

--Versão em duas linhas
qs :: Ord a => [a] -> [a]
qs []     = []
qs (x:xs) =  qsort [a | a <- xs, a < x] ++ [x] ++ qsort [b | b <- xs, b >= x]


even':: Int -> Bool 
even' 0 = True 
even' n = odd' (n-1)

odd' :: Int -> Bool 
odd' 0 = False 
odd' n = even' (n-1)

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x:odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs
 
   