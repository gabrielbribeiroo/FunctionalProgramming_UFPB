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



