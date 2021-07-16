{-# LANGUAGE DerivingStrategies #-}

module Vec where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Epsilon

-- Again copying Kmett, he forced args (!a !a !a !a) but I'm not going to
-- until I understand why he did it
data Vec a = Vec a a a a
  deriving stock (Eq, Ord, Show)

instance Functor Vec where
  fmap f (Vec a b c d) = Vec (f a) (f b) (f c) (f d)

-- this lets us do
-- > sqrt . getSum . foldMap Sum $ fmap (^2) v
-- which is a really verbose way of saying
-- > sqrt (a^2 + b^2 + c^2 + d^2)
-- but it might come in handy in a generalized case
instance Foldable Vec where
  foldMap f (Vec a b c d) = f a `mappend` f b `mappend` f c `mappend` f d

-- I don't really get Traversable so this is a good sandbox to play with
-- it isn't by itself
-- maybe I'll understand better with vecs-of-vecs matrices
instance Traversable Vec where
  traverse f (Vec a b c d) = Vec <$> f a <*> f b <*> f c <*> f d

instance Applicative Vec where
  pure a = Vec a a a a
  Vec a b c d <*> Vec e f g h = Vec (a e) (b f) (c g) (d h)

instance Num a => Num (Vec a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

quadrature :: Num a => Vec a -> a
quadrature (Vec a b c d) = a*a + b*b + c*c + d*d

instance (Epsilon a, Num a, Floating a) => Epsilon (Vec a) where
  zeroish = zeroish . quadrature

class Projective f where
  vec :: Num a => f a -> Vec a

scalar :: Num a => Vec a -> Vec a -> a
scalar (Vec a b c d) (Vec e f g h) = a*e + b*f + c*g + d*h