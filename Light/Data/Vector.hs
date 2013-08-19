{-# LANGUAGE TemplateHaskell #-}

module Light.Data.Vector
    -- ADT
    ( Vector, vector, dx, dy, dz

    -- Default Instances
    , zero, unitX, unitY, unitZ

    -- Arithmetic
    , (^+^), (^-^), (^.^), (*^), (^*), (^/)
    , negateV, magnitude, magnitudeSq, normalize, cross
    )
where

import Prelude hiding      (sum)
import Data.Foldable       (Foldable(..), sum)
import Control.Applicative (liftA2, Applicative(..))
import Control.Lens        ((^.))
import Control.Lens.TH     (makeLenses)

data Vector a = Vector { _dx :: !a, _dy :: !a, _dz :: !a }

vector = Vector

makeLenses ''Vector

instance (Show a) => Show (Vector a) where
  show (Vector x y z) = concat ["#V(", show x, ", ", show y, ", ", show z, ")"]

instance Functor Vector where
  fmap f (Vector x y z) = Vector (f x) (f y) (f z)

instance Applicative Vector where
  pure x = vector x x x
  (Vector fx fy fz) <*> (Vector x y z) = Vector (fx x) (fy y) (fz z)
  (*>) = flip const
  (<*) = const

instance Foldable Vector where
  foldr f s (Vector x y z) = x `f` (y `f` (z `f` s))

instance (Num a, Ord a, Fractional a) => Eq (Vector a) where
  u == v = magnitudeSq (u ^-^ v) < 0.00001

zero, unitX, unitY, unitZ :: (Num a) => Vector a
zero  = vector 0 0 0
unitX = vector 1 0 0
unitY = vector 0 1 0
unitZ = vector 0 0 1

(^+^), (^-^) :: (Num a) => Vector a -> Vector a -> Vector a
(^+^) = liftA2 (+)
(^-^) = liftA2 (-)

(^.^) :: (Num a) => Vector a -> Vector a -> a
u ^.^ v = sum $ liftA2 (*) u v

(^*) :: (Num a) => Vector a -> a -> Vector a
v ^* s = fmap (*s) v

(^/) :: (Num a, Fractional a) => Vector a -> a -> Vector a
v ^/ s = fmap (/s) v

(*^) = flip (^*)

negateV :: (Num a) => Vector a -> Vector a
negateV = fmap negate

magnitude = sqrt . magnitudeSq
magnitudeSq v = v ^.^ v

normalize v
 | magnitudeSq v == 0 = v
 | otherwise          = v ^/ magnitude v

cross u v = vector (u^.dy * v^.dz - u^.dz * v^.dy)
                   (u^.dz * v^.dx - u^.dx * v^.dz)
                   (u^.dx * v^.dy - u^.dy * v^.dx)
