{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e:el) (f + glf)
  where
    Emp { empFun = f } = e
    GL el glf = gl


instance Semigroup GuestList where
  GL l1 f1 <> GL l2 f2 = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 < f2   = gl2
  | otherwise = gl1

