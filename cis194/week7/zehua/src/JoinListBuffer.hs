{-# LANGUAGE FlexibleInstances #-}
module JoinListBuffer where

import           Data.Monoid ()

import           Buffer
import           JoinList
import           Scrabble
import           Sized

-- ex4
-- to avoid orphan instance warning
newtype BufferJoinList = BufferJoinList { getJoinList :: JoinList (Score, Size) String }

getJoinListScore :: JoinList (Score, Size) String -> Score
getJoinListScore Empty               = 0
getJoinListScore (Single (s, _) _)   = s
getJoinListScore (Append (s, _) _ _) = s

getJoinListSize :: JoinList (Score, Size) String -> Size
getJoinListSize Empty               = 0
getJoinListSize (Single _ _)        = 1
getJoinListSize (Append (_, s) _ _) = s

toLines :: JoinList (Score, Size) String -> [String]
toLines Empty            = []
toLines (Single _ s)     = [s]
toLines (Append _ j1 j2) = toLines j1 ++ toLines j2

jlToString :: JoinList (Score, Size) String -> String
jlToString = unlines . toLines

fromLines :: [String] -> JoinList (Score, Size) String
fromLines []  = Empty
fromLines [s] = Single ((scoreString s), 1) s
fromLines ls  = let l = length ls
                    (l1, l2) = splitAt ((l + 1) `div` 2) ls
                    j1 = fromLines l1
                    j2 = fromLines l2
                in j1 +++ j2

stringToJL :: String -> JoinList (Score, Size) String
stringToJL = fromLines . lines

replaceLineJL :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
replaceLineJL n l jl
    | lenAfter == 0 || lenPre /= n = jl
    | otherwise = pre +++ newL +++ post
  where
    pre  = takeJ n jl
    nAndAfter = dropJ n jl
    lenPre = getSize (getJoinListSize pre)
    lenAfter = getSize (getJoinListSize nAndAfter)
    newL = fromLines [l]
    post = dropJ 1 nAndAfter

instance Buffer BufferJoinList where
  toString        = jlToString . getJoinList
  fromString      = BufferJoinList . stringToJL
  line n          = indexJ n . getJoinList
  replaceLine n l = BufferJoinList . replaceLineJL n l . getJoinList
  numLines        = getSize . getJoinListSize . getJoinList
  value           = getScore . getJoinListScore . getJoinList

