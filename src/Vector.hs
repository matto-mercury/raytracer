{-# LANGUAGE DerivingStrategies #-}

module Vector
  (
  ) where

import Vec
import V3
import Epsilon

data Vector a = Vector { unVector :: V3 a }
  deriving (Eq, Show)
mkVector v = Vector { unVector = v }

instance Projective Vector where
  vec (Vector (V3 x y z)) = Vec x y z 0

cross :: Num a => Vector a -> Vector a -> Vector a
cross v0 v1 = xp
  where 
    V3 a b c = unVector v0
    V3 e f g = unVector v1
    xp = mkVector $ V3 (b*g - c*f) (a*g - c*e) (a*f - b*e)

len :: (Num a, Floating a) => Vector a -> a
len a = sqrt $ sqlen v
  where v = unVector a