{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Codensity
import Control.Monad.Trans

import Data.Field

main :: IO ()
main = print $ lowerCodensity @Field @Integer $ do
  x <- reset $ do
    a <- shift $ \k -> lift $ Mul (k 3) (k 5)
    b <- lift $ Pure 2
    lift $ Add (Pure a) (Pure b)
  lift $ Pure x
