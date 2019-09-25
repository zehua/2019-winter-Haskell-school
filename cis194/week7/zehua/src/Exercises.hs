{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Exercises where


import           JoinList
import           Sized

-- ex1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ a     = a
a     +++ Empty = a
j1    +++ j2    = Append (mappend m1 m2) j1 j2
  where
    m1 = tag j1
    m2 = tag j2


-- ex2
sizedTag :: (Sized m, Monoid m) => JoinList m a -> m
sizedTag = tag

getSizeFromTag :: (Sized m, Monoid m) => JoinList m a -> Int
getSizeFromTag = getSize . size . sizedTag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty     = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i j@(Append _ j1 j2)
  | i >= size0 = Nothing
  | i < size1  = indexJ i j1
  | otherwise  = indexJ (i - size1) j2
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty     = Empty
dropJ i a | i < 0 = a
dropJ i s@(Single _ _)
  | i == 0    = s
  | otherwise = Empty
dropJ i j@(Append _ j1 j2)
  | i >= size0 = Empty
  | i < size1  = let j1New   = dropJ i j1
                     sizeNew = mappend (sizedTag j1New) (sizedTag j2)
                 in Append sizeNew j1New j2
  | otherwise  = dropJ (i - size1) j2
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty     = Empty
takeJ i _ | i < 0 = Empty
takeJ i s@(Single _ _)
  | i == 1    = s
  | otherwise = Empty
takeJ i j@(Append _ j1 j2)
  | i >= size0 = j
  | i > size1  = let j2New   = takeJ (i - size1) j2
                     sizeNew = mappend (sizedTag j1) (sizedTag j2New)
                 in Append sizeNew j1 j2New
  | otherwise  = takeJ i j1
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1
