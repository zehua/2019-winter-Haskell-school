module ExercisesSpec where

import           Data.Monoid --(Product)
import           Control.Monad (mapM_)
import           Exercises
import           JoinList
import           Sized
import           Test.Hspec

singleProd :: Int -> String -> JoinList (Product Int) String
singleProd p = Single (Product p)

appendProd :: Int
           -> JoinList (Product Int) String
           -> JoinList (Product Int) String
           -> JoinList (Product Int) String
appendProd p = Append (Product p)

singleSize :: Int -> String -> JoinList Size String
singleSize p = Single (Size p)

appendSize :: Int
           -> JoinList Size String
           -> JoinList Size String
           -> JoinList Size String
appendSize p = Append (Size p)

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dataSize1 :: JoinList Size String
dataSize1 = singleSize 1 "abc"

dataSize2 :: JoinList Size String
dataSize2 = appendSize 2 (singleSize 1 "abc") (singleSize 1 "def")

dataSize4 :: JoinList Size String
dataSize4 = appendSize 4
              (singleSize 1 "abc")
              (appendSize 3
                (appendSize 2
                  (singleSize 1 "def")
                  (singleSize 1 "ghi"))
                (singleSize 1 "jkl"))

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works" $ do
      Empty +++ singleProd 1 "a" `shouldBe` singleProd 1 "a"
      singleProd 2 "b" +++ Empty `shouldBe` singleProd 2 "b"
      singleProd 2 "b" +++ singleProd 3 "cde" `shouldBe` appendProd 6 (singleProd 2 "b") (singleProd 3 "cde")

  describe "ex2" $ do
    it "works for indexJ that returns Nothing" $ do
      indexJ 0 (Empty :: JoinList Size String) `shouldBe` Nothing
      indexJ (-1) (singleSize 2 "abc") `shouldBe` Nothing
      indexJ 1 (singleSize 2 "abc") `shouldBe` Nothing
      indexJ 10 (appendSize 9 (singleSize 3 "abc") (singleSize 3 "def")) `shouldBe` Nothing

    it "works for indexJ" $ do
      mapM_ (\(i, jl) -> indexJ i jl `shouldBe` jlToList jl !!? i)
        [ (0, singleSize 2 "b")
        , (0, appendSize 2 (singleSize 1 "a") (singleSize 1 "b"))
        , (1, appendSize 2 (singleSize 1 "a") (singleSize 1 "b")) ]

    it "works for dropJ" $ do
      dropJ 0 (Empty :: JoinList Size String) `shouldBe` Empty
      dropJ (-1) (singleSize 2 "abc") `shouldBe` singleSize 2 "abc"
      dropJ (-2) (appendSize 9 (singleSize 3 "abc") (singleSize 3 "def"))
        `shouldBe` (appendSize 9 (singleSize 3 "abc") (singleSize 3 "def"))
      dropJ 0 dataSize1 `shouldBe` dataSize1
      dropJ 1 dataSize1 `shouldBe` Empty
      dropJ 1 dataSize2 `shouldBe` singleSize 1 "def"
      dropJ 0 dataSize4 `shouldBe` dataSize4
      dropJ 2 dataSize4 `shouldBe` (appendSize 2 (singleSize 1 "ghi") (singleSize 1 "jkl"))

    it "works for takeJ" $ do
      takeJ 0 (Empty :: JoinList Size String) `shouldBe` Empty
      takeJ (-1) dataSize1 `shouldBe` Empty
      takeJ (-2) dataSize2 `shouldBe` Empty
      takeJ 0 dataSize1 `shouldBe` Empty
      takeJ 1 dataSize1 `shouldBe` dataSize1
      takeJ 1 dataSize2 `shouldBe` dataSize1
      takeJ 2 dataSize2 `shouldBe` dataSize2
      takeJ 2 dataSize4 `shouldBe` dataSize2
      takeJ 4 dataSize4 `shouldBe` dataSize4
