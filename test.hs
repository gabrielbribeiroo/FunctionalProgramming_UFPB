double x = x + x

double' x = x * 2

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

-- remove the last element
my_last :: [a] -> a
my_last xs = head (reverse x)

my_init1 :: [a] -> a
my_init1 xs = take (length xs - 1) xs

my_init2 :: [a] -> a
my_init2 xs = reverse (tail (reverse xs))

-- extra 
insert z f [] = z
insert z f (x:xs) = f x (insert z f xs)