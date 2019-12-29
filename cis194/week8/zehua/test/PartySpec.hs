module PartySpec where

import           Data.Tree (rootLabel)
import           Employee
import           Party
import           Test.Hspec


testBoss :: Employee
testBoss = rootLabel testCompany

testEmp1 :: Employee
testEmp1 = Emp "employee 1" 5

testEmp2 :: Employee
testEmp2 = Emp "employee 2" 7

-- build a singleton GuestList with one employee
sGL :: Employee -> GuestList
sGL e = glCons e mempty

testGL1n2 :: GuestList
testGL1n2 = sGL testEmp1 <> sGL testEmp2

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works with glCons" $ do
      glCons testBoss testGL1n2 `shouldBe` GL [testBoss, testEmp1, testEmp2] 21

    it "works with monoid" $ do
      sGL testEmp1 <> sGL testEmp2 <> sGL testBoss `shouldBe` GL [testEmp1, testEmp2, testBoss] 21

    it "works with moreFun" $ do
      moreFun (sGL testEmp1) (sGL testEmp2) `shouldBe` sGL testEmp2
      moreFun (sGL testEmp2) (sGL testEmp1) `shouldBe` sGL testEmp2
      moreFun (sGL testEmp2) (sGL testBoss) `shouldBe` sGL testBoss
      moreFun testGL1n2 (sGL testBoss) `shouldBe` testGL1n2

