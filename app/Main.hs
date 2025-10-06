{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Codensity
import Control.Monad.Trans

import Data.Field
import Data.Maybe

{-|
  WARNING: The reciprocal is undefined for zero.
-}
rcp' :: Field a -> Field a
rcp' = fromMaybe zero . rcp

expr :: (Eq a, Enum a, Num a) => Field a
expr = mul x y
  where
    x = rcp' $ num 2
    y = lowerCodensity $ do
      a <- Codensity $ \k -> flatMapSum k [0..3]
      b <- Codensity $ \k -> mul (k 2) (k 4)
      lift $ add (num a) (num b)

main :: IO ()
main = print (expr @Integer)
