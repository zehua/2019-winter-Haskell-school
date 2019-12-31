module RiskSpec where

import           Control.Monad        (replicateM_)
import           Control.Monad.Random (evalRand, evalRandIO)
import           Data.Ix              (inRange)
import           Risk
import           System.Random        (RandomGen (genRange, next, split))
import           Test.Hspec


-- a mock to simulate random generation with a pre-defined sequence of "random" numbers
-- we only support one single list of values, once used up _|_ will be returned
newtype MockRandomGen = MockRandomGen { getValues :: [Int] }
  deriving Show

instance RandomGen MockRandomGen where
  next g = case getValues g of
             (n:ns) -> (n, MockRandomGen ns)
             _      -> undefined
  genRange _ = (1, 6)
  split = undefined

mkMockRandomGen :: [Int] -> MockRandomGen
-- inserting [1,1,1,1] in between each desired random output as `random` calls `next` multiple times
-- and it so happens that it takes 5 calls to `next` for it to be happy when genRange is (1,6)
-- see https://hackage.haskell.org/package/random-1.1/docs/src/System.Random.html#randomIvalInteger
mkMockRandomGen = MockRandomGen . concat . map (\x -> [1,1,1,1,x])

rollsShouldBeValid :: [DieValue] -> Int -> Expectation
rollsShouldBeValid rs n = do
  length rs `shouldBe` n
  -- ensure in range
  mapM_ (\a -> a `shouldSatisfy` inRange (1, 6) . unDV ) rs
  -- ensure sorted desc
  mapM_ (\a -> a `shouldSatisfy` uncurry (>=)) $ zip rs (tail rs)

bf :: Army -> Army -> Battlefield
bf a d = Battlefield { attackers = a, defenders = d }


spec :: Spec
spec = do
  describe "ex1" $ do
    it "works for dieNSorted via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5,6]) $ do
        r1 <- dieNSorted 1
        r2 <- dieNSorted 3
        r3 <- dieNSorted 2
        return (\() -> do
          r1 `shouldBe` [1]
          r2 `shouldBe` [4,3,2]
          r3 `shouldBe` [6,5]
          )

    it "works for dieNSorted with StdGen via evalRandIO" $ do
      -- repeating more times in a hope to cover the full range
      replicateM_ 100 $ do
        actual <- evalRandIO $ dieNSorted 1
        actual `rollsShouldBeValid` 1
      replicateM_ 100 $ do
        actual <- evalRandIO $ dieNSorted 2
        actual `rollsShouldBeValid` 2
      replicateM_ 100 $ do
        actual <- evalRandIO $ dieNSorted 3
        actual `rollsShouldBeValid` 3

  describe "ex2" $ do
    it "works for updateBattlefield" $ do
      updateBattlefield (bf 3 3) [(6, 5)] `shouldBe` bf 3 2
      updateBattlefield (bf 3 3) [(6, 1)] `shouldBe` bf 3 2
      updateBattlefield (bf 3 3) [(3, 3)] `shouldBe` bf 2 3
      updateBattlefield (bf 3 3) [(3, 6)] `shouldBe` bf 2 3
      updateBattlefield (bf 3 3) [(6, 5), (5, 3)] `shouldBe` bf 3 1
      updateBattlefield (bf 3 3) [(6, 5), (5, 3), (3, 2)] `shouldBe` bf 3 0
      updateBattlefield (bf 3 3) [(5, 5), (1, 2)] `shouldBe` bf 1 3
      updateBattlefield (bf 3 3) [(5, 5), (3, 3), (1, 2)] `shouldBe` bf 0 3
      updateBattlefield (bf 3 3) [(6, 5), (3, 3)] `shouldBe` bf 2 2
      updateBattlefield (bf 3 3) [(6, 5), (3, 3), (3, 2)] `shouldBe` bf 2 1
      updateBattlefield (bf 3 3) [(6, 5), (3, 3), (1, 2)] `shouldBe` bf 1 2
      updateBattlefield (bf 3 3) [(1,3), (6, 5), (3, 3), (1, 2), (4, 1), (6, 1)] `shouldBe` bf 0 0

    it "works for dispatchAttackers" $ do
      dispatchAttackers (bf 0 3) `shouldBe` 0
      dispatchAttackers (bf 1 3) `shouldBe` 0
      dispatchAttackers (bf 2 3) `shouldBe` 1
      dispatchAttackers (bf 3 3) `shouldBe` 2
      dispatchAttackers (bf 4 3) `shouldBe` 3
      dispatchAttackers (bf 5 3) `shouldBe` 3
      dispatchAttackers (bf 10 3) `shouldBe` 3

    it "works for dispatchDefenders" $ do
      dispatchDefenders (bf 4 0) `shouldBe` 0
      dispatchDefenders (bf 4 1) `shouldBe` 1
      dispatchDefenders (bf 4 2) `shouldBe` 2
      dispatchDefenders (bf 4 3) `shouldBe` 2
      dispatchDefenders (bf 4 9) `shouldBe` 2

    it "works for battle via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5, 6,3,4,5,3, 6,3,4,3,6, 3,1,4, 1,3,2, 6, 5]) $ do
        r1 <- battle $ bf 4 4 -- a: 3,2,1 d: 5,4
        r2 <- battle $ bf 4 4 -- a: 6,4,3 d: 5,3
        r3 <- battle $ bf 4 4 -- a: 6,4,3 d: 6,3
        r4 <- battle $ bf 2 4 -- a: 3 d: 4,1
        r5 <- battle $ bf 3 1 -- a: 3,1 d: 2
        r6 <- battle $ bf 2 0 -- a: 6 d:
        r7 <- battle $ bf 1 1 -- a:   d: 5
        return (\() -> do
          r1 `shouldBe` bf 2 4
          r2 `shouldBe` bf 4 2
          r3 `shouldBe` bf 3 3
          r4 `shouldBe` bf 1 4
          r5 `shouldBe` bf 3 0
          r6 `shouldBe` bf 2 0
          r7 `shouldBe` bf 1 1
          )
