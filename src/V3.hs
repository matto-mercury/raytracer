{-# LANGUAGE DerivingStrategies #-}

module V3 
  ( V3 (..)
  , sqlen
  ) where

import Data.Functor
import Control.Applicative
import Vec
import Epsilon

data V3 a = V3 a a a
  deriving (Eq, Show)

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)

instance Applicative V3 where
  pure a = V3 a a a
  V3 a b c <*> V3 e f g = V3 (a e) (b f) (c g)

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

sqlen :: Num a => V3 a -> a
sqlen (V3 a b c) = a*a + b*b + c*c

instance (Epsilon a, Num a, Floating a) => Epsilon (V3 a) where
  zeroish = zeroish . sqlen

