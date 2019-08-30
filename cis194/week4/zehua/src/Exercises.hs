module Exercises where

import           Data.Bool (bool)
import           Data.List (group, sort)

-- ex1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . fmap (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldr (+) 0
      . filter even
      . takeWhile (>1) -- >1 instead of /=1 to defend against negative numbers
      . iterate (bool <$> oddv <*> evenv <*> even)
    where
      oddv = (+1) . (*3)
      evenv = (`div` 2)


-- ex2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf            = -1
getHeight (Node h1 _ _ _) = h1

foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
  where
    f x Leaf = Node 0 Leaf x Leaf
    f x (Node h l n r)
        | hl == hr  = let l'  = f x l
                          hl' = getHeight l'
                          h'  = bool h (h+1) (hl < hl')
                      in Node h' l' n r
        | hl < hr   = Node h (f x l) n r
        | otherwise = Node h l n (f x r)
      where
        hl = getHeight l
        hr = getHeight r


-- ex3
xor2 :: Bool -> Bool -> Bool
xor2 = (/=)

xor :: [Bool] -> Bool
xor = foldr xor2 False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse


-- ex4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2))
                . concat
                . filter ((==1) . length)
                . group
                . sort
                . ([1..n] ++)
                . filter (<=n)
                . map (\(i, j) -> i + j + 2 * i * j)
                . filter (uncurry (<=))
                $ cartProd [1..n] [1..n]
