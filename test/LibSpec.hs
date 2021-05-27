module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Lib

products :: [Product]
products = [
   Product (ProductId 1) (Price 10) Blue,
   Product (ProductId 2) (Price 3) White,
   Product (ProductId 3) (Price 4) Green,
   Product (ProductId 4) (Price 5) White,
   Product (ProductId 5) (Price 1) White]

spec :: Spec
spec = describe "sortProductsOneDim" $ do

   it "sort by color white" $
     fmap pid (sortProductsOneDim (SortByColor White) products)
       `shouldBe` fmap ProductId [2, 4, 5, 1, 3]

   it "sort by color white and price" $
     fmap pid (sortProducts [SortByColor White, SortByPrice] products)
       `shouldBe` fmap ProductId [5, 2, 4, 3, 1]

   prop "property-based unit test" $
     \l -> reverse ( reverse l ) == ( l::[Int])
