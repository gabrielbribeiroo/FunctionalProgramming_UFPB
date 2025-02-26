-- :module +Data.List

-- Expressões condicionais

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 then -1 
            else if n == 0 then 0 
                 else 1


-- Equações protegidas (guarded)

abs_g n | n >= 0    = n
        | otherwise = -n

signum_g n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1


-- Casamento de padrões

not_p :: Bool -> Bool
not_p False = True
not_p True  = False

(&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _    && _    = False
True  && b = b
False && _ = False

-- Padrões de listas

-- 1:(2:(3:(4:[])))
-- 1:2:3:4:[]

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs


-- Expressões lambda

twice f x = f (f x)
--Exemplo lambda
-- twice (\x -> x + x) 8 

add :: Int -> Int -> Int
--add x y = x + y
add = \x -> (\y -> x + y)

odds :: (Num a, Enum a) => a -> [a]
odds n = map f [0..n-1]
         where
            f x = x*2 + 1
--odds n = map (\x -> x*2 + 1) [0..n-1]


--Seções de operador

--Exemplo de função sucessora
inc = (+) 1 

--Exemplo de função de reciprocidade
reciprocidade = (1/) 

--Exemplo de função de duplicação
dup = (2*) 

--Exemplo de função de redução pela metade
met = (/2) 


-- exercícios

third [a] -> a
-- third xs = head (tail (tail xs))
-- third xs = xs !! 2
third (_:_:x:_) = x

safetail :: [a] -> [a]
-- safetail xs = if null then xs else tail xs
-- safetail xs | null xs = [] 
--             | otherwise = tail xs
safetail [] = []
safetail (_:xs) = xs

luhnDouble :: Int -> Int
luhnDouble x = if doubled > 9 then doubled - 9 else doubled
        where doubled = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn a b c d = total `mod` 10 == 0
        where total = luhnDouble a + b + luhnDouble c + d