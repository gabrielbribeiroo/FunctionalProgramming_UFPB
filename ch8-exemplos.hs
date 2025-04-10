-- Exemplos do Capítulo 8


-- 8.1 Type declarations


type String = [Char]

type Pos = (Int,Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m,n) = m*n

copy :: a -> Pair a
copy x = (x,x)

type Trans = Pos -> Pos

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v 
find k t = head [v | (k',v) <- t, k == k']



-- 8.2 Data declarations

data Answer = Yes | No | Unknown

instance Show Answer where
    show     Yes = "Yes"
    show     No  = "No"
    show Unknown = "Unknown"

answers :: [Answer]
answers = [Yes,No,Unknown]

flipp :: Answer -> Answer
flipp Yes     = No
flipp No      = Yes
flipp Unknown = Unknown

data Shape = Circle Float
           | Rect Float Float

instance Show Shape where
    show (Circle r) = "Circle(" ++ show r++")"
    show (Rect x y) = "Rect(" ++ show x ++"," ++ show y ++")"

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- :type Circle
-- :type Float

data Perhaps a = Nihil | Only a

instance Show a => Show (Perhaps a) where
    show Nihil    = "Nihil"
    show (Only a) = "Only("++ show a ++")"  


safediv :: Int -> Int -> Perhaps Int
safediv _ 0 = Nihil
safediv m n = Only (m `div` n)

safehead :: [a] -> Perhaps a
safehead [] = Nihil
safehead xs = Only (head xs)


-- 8.4 Recursive types

data Nat = Zero | Succ Nat
--   deriving Show 
--instance Show Nat where
--        show Zero    = "Zero"
--        show (Succ n) = "Succ("++ show n ++")" 

instance Show Nat where
    show Zero    = "0"
    show (Succ n) = show (1 + (nat2int n)) 


nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add' :: Nat -> Nat -> Nat
add'	 m n = int2nat (nat2int m + nat2int n)

add Zero     n = n
add (Succ m) n = Succ (add m n)

--(1 + m) * n = (n + m*n) = m*n + n

mult' :: Nat -> Nat -> Nat
mult' Zero     n = Zero
mult' (Succ m) n = add (mult' m n) n 

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
   deriving Show
--instance Show Expr where
--    show (Val n)    = "Val " ++ show n
--    show (Add n m) = "Add ("++ show n ++")" ++" "++ "("++ show m ++")"
--    show (Mul n m) = "Mul ("++ show n ++")" ++" "++ "("++ show m ++")"

-- Add (Val 1) (Mul (Val 2) (Val 3))

size :: Expr -> Int
size (Val n)   = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y 

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


data List a = Nil | Cons a (List a)
       deriving Show

primes :: List Int
primes = Cons 2 (Cons 3 (Cons 5 (Cons 7 Nil)))

len :: List a -> Int 
len Nil         = 0 
len (Cons _ xs) = 1 + len xs



data Tree a = Leaf a | Node (Tree a) a (Tree a)
       deriving Show


t :: Tree Int 
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y 
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r 

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                 = x == y 
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r

-- 8.6 Tautology checker

data Prop =   Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop

instance Show Prop where
    show (Const b)      = show b
    show (Var ch)       = show ch
    show (Not p)        = " not " ++ (show p)
    show (And p1 p2)    = (show p1) ++ " ^ " ++ (show p2)
    show (Imply p1 p2)  = (show p1) ++ " => " ++ (show p2)



p1 :: Prop 
p1 = And (Var 'A') (Not (Var 'A'))
 
p2 :: Prop 
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
 
p3 :: Prop 
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
 
p4 :: Prop 
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

memory :: Subst
memory = [('A',False),('B',True)]

eval' :: Subst -> Prop -> Bool
eval' _ (Const b)    = b
eval' s (Var x)      = find x s 
eval' s (Not p)      = not (eval' s p)
eval' s (And p q)    = eval' s p && eval' s q
eval' s (Imply p q)  = eval' s p <= eval' s q

vars :: Prop -> [Char]
vars (Const b)    = []
vars (Var x)      = [x] 
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range 
  where range      = [0..(2^n)-1]
        make n bs  = take n (bs ++ repeat 0)
        conv 0     = False
        conv 1     = True
        int2bin 0  = [] 
        int2bin n  = n `mod` 2 : int2bin (n `div` 2)

bools' :: Int -> [[Bool]] 
bools' 0 = [[]] 
bools' n = map (False:) bss ++ map (True:) bss 
  where bss = bools' (n-1)


rmdups :: Eq a => [a] -> [a] 
rmdups [] = [] 
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst] 
substs p = map (zip vs) (bools (length vs)) 
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool 
isTaut p = and [eval' s p | s <- substs p]        

-- 8.7 Abstract machine

data Expr' = Val' Int | Add' Expr' Expr'

instance Show Expr' where
      show (Val' i)         = show i
      show (Add' exp1 exp2) = "(" ++ (show exp1) ++ " + " ++ (show exp2) ++ ")"

plus234 = Add' (Add' (Val' 2) (Val' 3)) (Val' 4)

value :: Expr' -> Int 
value (Val' n)= n 
value (Add' x y) = value x + value y

-- Abstract machine for expressions evaluation

type Cont = [Op]

data Op = EVAL Expr' | ADD Int

eval'' :: Expr' -> Cont -> Int
eval'' (Val' n)   c = exec c n
eval'' (Add' x y) c = eval'' x (EVAL y : c)

exec :: Cont -> Int -> Int 
exec []           n = n
exec (EVAL y : c) n = eval'' y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

value' :: Expr' -> Int 
value' e = eval'' e []

{-
  value (Add (Add (Val 2) (Val 3)) (Val 4)) 
= { applying value } 
  eval (Add (Add (Val 2) (Val 3)) (Val 4)) [] 
= { applying eval } 
  eval (Add (Val 2) (Val 3)) [EVAL (Val 4)]
= { applying eval } 
  eval (Val 2) [EVAL (Val 3), EVAL (Val 4)] 
= { applying eval } 
  exec [EVAL (Val 3), EVAL (Val 4)] 2 
= { applying exec } 
  eval (Val 3) [ADD 2, EVAL (Val 4)] 
= { applying eval }
  exec [ADD 2, EVAL (Val 4)] 3 
= { applying exec } 
  exec [EVAL (Val 4)] 5 
= { applying exec } 
  eval (Val 4) [ADD 5] 
= { applying eval } 
  exec [ADD 5] 4 
= { applying exec } 
  exec [] 9 
= { applying exec }
  9
-}