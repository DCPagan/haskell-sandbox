{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Codensity
import Control.Monad.Trans

import Data.Field

expr :: Field Integer
expr = Mul x y
  where
    x = MulInverse $ Pure 2
    y = lowerCodensity @Field @Integer $ do
      a <- Codensity $ \k -> flatMapSumField k [0..3]
      b <- Codensity $ \k -> mul (k 2) (k 4)
      lift $ Add (Pure a) (Pure b)

main :: IO ()
main = print expr
