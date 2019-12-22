module JoinList where


import           Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
