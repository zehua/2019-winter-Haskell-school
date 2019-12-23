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

foldJoinListScoreSize :: a -> (JoinList (Score, Size) String -> a) -> (JoinList (Score, Size) String -> a) -> JoinList (Score, Size) String -> a
foldJoinListScoreSize e _ _ Empty                 = e
foldJoinListScoreSize _ i _ s@(Single (_, _) _)   = i s
foldJoinListScoreSize _ _ a j@(Append (_, _) _ _) = a j

getJoinListScoreDirect :: JoinList (Score, Size) String -> Score
getJoinListScoreDirect = fst . tag

getJoinListScoreFold :: JoinList (Score, Size) String -> Score
getJoinListScoreFold = foldJoinListScoreSize 0 (fst . tag) (fst . tag)

getJoinListScore :: JoinList (Score, Size) String -> Score
getJoinListScore = getJoinListScoreFold

getJoinListSizeDirect :: JoinList (Score, Size) String -> Size
getJoinListSizeDirect Empty               = 0
getJoinListSizeDirect (Single _ _)        = 1
getJoinListSizeDirect (Append (_, s) _ _) = s

getJoinListSizeFold :: JoinList (Score, Size) String -> Size
getJoinListSizeFold = foldJoinListScoreSize 0 (const 1) (snd . tag)

getJoinListSize :: JoinList (Score, Size) String -> Size
getJoinListSize = getJoinListSizeFold

toLinesDirect :: JoinList (Score, Size) String -> [String]
toLinesDirect Empty            = []
toLinesDirect (Single _ s)     = [s]
toLinesDirect (Append _ j1 j2) = toLinesDirect j1 ++ toLinesDirect j2

toLinesFold :: JoinList (Score, Size) String -> [String]
toLinesFold = foldJoinListScoreSize [] i a
  where
    i (Single _ s) = [s]
    i _            = []
    a (Append _ j1 j2) = toLinesFold j1 ++ toLinesFold j2
    a _                = []

toLines :: JoinList (Score, Size) String -> [String]
toLines = toLinesFold

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

