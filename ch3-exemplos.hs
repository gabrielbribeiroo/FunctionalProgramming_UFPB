add :: (Int,Int) -> Int
add (x,y) = x+y

zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> Int -> Int
add' x y = x+y

add_10 = add' 10

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

mult_10 :: Int -> Int -> Int
mult_10 = mult 10

mult_10_5 :: Int -> Int
mult_10_5 = mult 10 5

add_1 :: Int -> Int
add_1 n = add' 1 n


take_5 :: [Int] -> [Int]
take_5 xs = take 5 xs


drop_5 :: [Int] -> [Int]
drop_5 xs = drop 5 xs


copy :: a -> (a,a) 
copy x = (x, x)


apply :: (a -> b) -> a -> b 
apply (*5) 2 


second :: [a] -> a
second xs = head (tail xs)


swap :: (Int, Int) -> (Int, Int)
swap (x,y) = (y,x)


pair :: Int -> Int -> (Int, Int)
pair x y = (x,y)


palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs 


twice :: (a -> a) -> a -> a
twice f x = f (f x) 