{-# LANGUAGE DerivingStrategies #-}

module Point
  (
  ) where

import Vec
import V3
import Vector

newtype Point a = Point { unPoint :: V3 a }
  deriving (Eq, Show)
mkPoint v = Point { unPoint = v }

instance Projective Point where
  vec (Point (V3 x y z)) = Vec x y z 1

(+.) :: Num a => Vector a -> Point a -> Point a
(+.) v p = Point sum
  where
    a = unVector v
    b = unPoint p
    sum = a + b

(.+) :: Num a => Point a -> Vector a -> Point a
(.+) p v = Point sum
  where
    a = unPoint p
    b = unVector v
    sum = a + b

(.-) :: Num a => Point a -> Vector a -> Point a
(.-) p v = Point diff
  where
    a = unPoint p
    b = unVector v
    diff = a - b