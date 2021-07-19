{-# LANGUAGE DerivingStrategies #-}

module Geom.Vector where

import Math.Vec as V
import Math.V3
import Math.Epsilon
import Data.Monoid
import Data.Group

newtype Vector a = Vector { unVector :: V3 a }
  deriving (Eq, Show)
mkVector v = Vector { unVector = v }

instance Projective Vector where
  vec (Vector (V3 x y z)) = Vec x y z 0

dot :: Num a => Vector a -> Vector a -> a
dot v0 v1 = dp
  where
    a = vec v0
    b = vec v1
    dp = a `V.scalar` b

cross :: Num a => Vector a -> Vector a -> Vector a
cross v0 v1 = xp
  where 
    V3 a b c = unVector v0
    V3 e f g = unVector v1
    xp = mkVector $ V3 (b*g - c*f) (a*g - c*e) (a*f - b*e)

len :: (Num a, Floating a) => Vector a -> a
len a = sqrt $ sqlen v
  where v = unVector a

-- can't make these (+) and (-) because Num owns those operators, and Vector
-- won't have an instance of Num
(+|) :: (Num a) => Vector a -> Vector a -> Vector a
(+|) v0 v1 = Vector vsum
  where
    a = unVector v0
    b = unVector v1
    vsum = a + b
    
(-|) :: (Num a) => Vector a -> Vector a -> Vector a
(-|) v0 v1 = Vector vsub
  where
    a = unVector v0
    b = unVector v1
    vsub = a - b

(*|) :: (Num a) => a -> Vector a -> Vector a
(*|) n v = Vector scaled
  where
    a = unVector v
    scaled = fmap (n*) a