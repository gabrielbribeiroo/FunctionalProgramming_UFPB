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
 

--exercícios
(^) :: Int -> Int -> Int 
_ ^ 0 = 1 
x ^ n = x * (x ^ (n - 1))

and :: [Bool] -> Bool 
and [] = True                -- Uma lista vazia é considerada verdadeira 
and (x:xs) = x && and xs    -- O resultado é True se x for True e o resto da lista também for 

concat :: [[a]] -> [a] 
concat [] = []                  -- Uma lista vazia retorna uma lista vazia 
concat (xs:xss) = xs ++ concat xss   -- Junta a primeira lista com a concatenação do restante

insert :: Ord a => a -> [a] -> [a] 
insert x [] = [x]     -- Caso base: inserir em uma lista vazia resulta em uma lista com apenas x 
insert x (y:ys) 
   | x <= y    = x : y : ys    -- Se x for menor ou igual a y, insere antes de y 
   | otherwise = y : insert x ys   -- Caso contrário, continua a busca recursiva

isort :: Ord a => [a] -> [a] 
isort [] = []               -- Caso base: lista vazia já está ordenada 
isort (x:xs) = insert x (isort xs)   -- Insere o primeiro elemento na lista ordenada restante