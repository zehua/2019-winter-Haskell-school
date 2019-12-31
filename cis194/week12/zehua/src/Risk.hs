{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sortBy)
import Data.Ord (comparing, Down(Down))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- make gen generic to help testing
-- die :: Rand StdGen DieValue
die :: RandomGen g => Rand g DieValue
die = getRandom

-- ex1
sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (comparing Down)

-- roll n times
dieNSorted :: RandomGen g => Int -> Rand g [DieValue]
dieNSorted n = fmap sortDesc $ replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Show)

-- ex2
dispatchAttackers :: Battlefield -> Army
dispatchAttackers = max 0 . min 3 . (+ (-1)) . attackers
{-
dispatchAttackers bf
  | a <= 0    = 0
  | otherwise = min 3 a
  where
    a = attackers bf - 1
-}

dispatchDefenders :: Battlefield -> Army
dispatchDefenders = min 2 . defenders

-- update a battlefield based on results of die
updateBattlefield :: Battlefield -> [(DieValue, DieValue)] -> Battlefield
updateBattlefield bf [] = bf
updateBattlefield bf ((ar, dr):rs) = updateBattlefield bf1 rs
  where bf1 = if ar > dr
                 then bf { defenders = defenders bf - 1 }
                 else bf { attackers = attackers bf - 1 }

-- battle :: Battlefield -> Rand StdGen Battlefield
battle :: RandomGen g => Battlefield -> Rand g Battlefield
battle bf = do
  ar <- dieNSorted $ dispatchAttackers bf
  dr <- dieNSorted $ dispatchDefenders bf
  let pairs = zip ar dr
  return $ updateBattlefield bf pairs
