{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Codensity
import Control.Monad.Trans

import Data.Field

expr :: Field Integer
expr = mul x y
  where
    x = MulInverse $ num 2
    y = lowerCodensity @Field @Integer $ do
      a <- Codensity $ \k -> flatMapSum k [0..3]
      b <- Codensity $ \k -> mul (k 2) (k 4)
      lift $ add (num a) (num b)

main :: IO ()
main = print expr
