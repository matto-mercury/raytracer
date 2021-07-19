module Math.Epsilon 
  ( Epsilon(..)
  ) where

-- basically a stripped-down version of Kmett's
class Num a => Epsilon a where
  zeroish :: a -> Bool

-- "stripped down" in that I only have these instances, and I'm probably going
-- to remove one of them for the raytracer... this doesn't have to be general
-- purpose
instance Epsilon Float where
  zeroish a = abs a <= 1e-6

instance Epsilon Double where
  zeroish a = abs a <= 1e-12