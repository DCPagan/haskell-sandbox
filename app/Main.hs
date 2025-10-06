{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Codensity
import Control.Monad.Trans

import Data.Field

main :: IO ()
main = print $ lowerCodensity @Field @Integer $ do
  x <- reset $ do
    a <- shift $ \k -> lift $ Mul (k 3) (k 5)
    let b = MulInverse $ Pure 2
    lift $ Mul (Pure a) b
  lift $ Pure x
