double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns


-- extra 

insert z f [] = z
insert z f (x:xs) = f x (insert z f xs)

