{-# LANGUAGE DerivingStrategies #-}

module Geom.Point where

import Math.Vec
import Math.V3
import Geom.Vector

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
(.+) = flip (+.)

(.-) :: Num a => Point a -> Vector a -> Point a
(.-) p v = Point diff
  where
    a = unPoint p
    b = unVector v
    diff = a - b