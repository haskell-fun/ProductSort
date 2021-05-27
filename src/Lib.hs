module Lib where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype ProductId = ProductId Int deriving (Eq, Show)
newtype Price = Price Double deriving (Eq, Show, Ord)
data Color = Red | Green | Blue | White | Black deriving (Eq, Show)

data Product = Product {
  pid :: ProductId,
  price :: Price,
  color :: Color
} deriving (Eq, Show)
-- data Product = Product ProductId Price Color deriving (Eq, Show)
-- pid :: Product -> ProductId
-- pid (Product pid _ _) = pid

data SortBy = SortByPrice | SortByColor Color deriving (Eq, Show)

sortProductsOneDim :: SortBy -> [Product] -> [Product]
sortProductsOneDim SortByPrice = sortOn price
sortProductsOneDim (SortByColor c) = sortOn f where
  f :: Product -> Int
  f = g . color
  g :: Color -> Int
  g c' | c' == c = 1
       | otherwise = 2

sortProducts :: [SortBy] -> [Product] -> [Product]
sortProducts bys ps = foldr sortProductsOneDim ps bys
