{-# LANGUAGE DerivingStrategies #-}

module Point
  (
  ) where

import Vec
import V3
import Vector

data Point a = Point { unPoint :: V3 a }
  deriving (Eq, Show)
mkPoint v = Point { unPoint = v }

instance Projective Point where
  vec (Point (V3 x y z)) = Vec x y z 1