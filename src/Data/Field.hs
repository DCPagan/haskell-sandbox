{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.Field
  ( Field(..)
  , FieldF(..)
  , eval
  , formatPrefix
  , formatTree) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Field a where
  Pure :: a -> Field a
  AddUnit :: Field a
  MulUnit :: Field a
  AddInverse :: Field a -> Field a
  MulInverse :: Field a -> Field a
  Add :: Field a -> Field a -> Field a
  Mul :: Field a -> Field a -> Field a
  deriving (Eq, Functor, Foldable, Traversable)

makeBaseFunctor ''Field

instance Applicative Field where
  pure = Pure
  Pure f <*> a = f <$> a
  AddUnit <*> _ = AddUnit
  MulUnit <*> _ = MulUnit
  AddInverse f <*> a = AddInverse $ f <*> a
  MulInverse f <*> a = MulInverse $ f <*> a
  Add a b <*> c = Add (a <*> c) (b <*> c)
  Mul a b <*> c = Mul (a <*> c) (b <*> c)

instance Monad Field where
  Pure a >>= k = k a
  AddUnit >>= _ = AddUnit
  MulUnit >>= _ = MulUnit
  AddInverse a >>= k = AddInverse $ a >>= k
  MulInverse a >>= k = MulInverse $ a >>= k
  Add a b >>= k = Add (a >>= k) (b >>= k)
  Mul a b >>= k = Mul (a >>= k) (b >>= k)

eval :: (Fractional a, Eq a) => Field (Maybe a) -> Maybe a
eval = cata $ \case
  PureF a -> a
  AddUnitF -> Just 0
  MulUnitF -> Just 1
  AddInverseF a -> negate <$> a
  MulInverseF a -> if a == Just 0
    then Nothing
    else recip <$> a
  AddF a b -> liftA2 (+) a b
  MulF a b -> liftA2 (*) a b

cataM :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a)
  -> t
  -> m a
cataM alg = go
  where
    go = alg <=< traverse go . project

anaM :: (Corecursive t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))
  -> a
  -> m t
anaM coalg = go
  where
    go = fmap embed . traverse go <=< coalg

hyloM :: (Traversable f, Monad m) => (f b -> m b) -> (a -> m (f a)) -> a -> m b
hyloM alg coalg = go
  where
    go = alg <=< traverse go <=< coalg

-- |tabstop
ts :: Word
ts = 2

indent :: (MonadReader Word m, MonadWriter String m) => m ()
indent = asks (flip replicate ' ' . fromIntegral . (ts *)) >>= tell

newline :: (MonadReader Word m, MonadWriter String m) => m ()
newline = tell "\n" >> indent

formatNodePrefix :: (MonadReader Word m, MonadWriter String m)
  => String
  -> m ()
  -> m ()
  -> m ()
formatNodePrefix op a b = do
  tell "("
  tell op
  tell " "
  a
  tell " "
  b
  tell ")"

formatNodeTree :: (MonadReader Word m, MonadWriter String m)
  => String
  -> m ()
  -> m ()
  -> m ()
formatNodeTree op a b = local succ $ do
  tell op
  newline
  a
  newline
  b

formatPrefix :: (Show a) => Field a -> String
formatPrefix = flip execWriterT "" . flip runReaderT 0 . cata f
  where
    f :: (Show a, MonadReader Word m, MonadWriter String m)
      => FieldF a (m ())
      -> m ()
    f = \case
      PureF a -> tell $ show a
      AddUnitF -> tell "0"
      MulUnitF -> tell "1"
      AddInverseF a -> tell "-" >> a
      MulInverseF a -> tell "/" >> a
      AddF a b -> formatNodePrefix "+" a b
      MulF a b -> formatNodePrefix "*" a b

formatTree :: (Show a) => Field a -> String
formatTree = flip execWriterT "" . flip runReaderT 0 . cata f
  where
    f :: (Show a, MonadReader Word m, MonadWriter String m)
      => FieldF a (m ())
      -> m ()
    f = \case
      PureF a -> tell $ show a
      AddUnitF -> tell "0"
      MulUnitF -> tell "1"
      AddInverseF a -> tell "-" >> a
      MulInverseF a -> tell "/" >> a
      AddF a b -> formatNodeTree "+" a b
      MulF a b -> formatNodeTree "*" a b

instance (Show a) => Show (Field a) where
  show = formatPrefix
