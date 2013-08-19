{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.Point
    -- ADT
	( Point, point, x, y, z

	-- Default Instances
    , origin

	-- Arithmetic
	, (.-.), (.+^), (.-^), distance, distanceSq
	)
where

import Data.Foldable       (Foldable(..))
import Control.Applicative (Applicative(..))
import Control.Lens        ((^.))
import Control.Lens.TH     (makeLenses)
import Light.Data.Vector   (Vector, vector, dx, dy, dz, magnitude, magnitudeSq)

data Point a = Point { _x :: !a, _y :: !a, _z :: !a }

point = Point

makeLenses ''Point

instance (Show a) => Show (Point a) where
  show (Point x y z) = concat ["#P(", show x, ", ", show y, ", ", show z, ")"]

instance Functor Point where
  fmap f (Point x y z) = Point (f x) (f y) (f z)

instance Applicative Point where
  pure x = point x x x
  (Point fx fy fz) <*> (Point x y z) = Point (fx x) (fy y) (fz z)
  (*>) = flip const
  (<*) = const

instance Foldable Point where
  foldr f s (Point x y z) = x `f` (y `f` (z `f` s))

instance (Num a, Ord a, Fractional a) => Eq (Point a) where
  u == v = distanceSq u v < 0.00001

origin :: (Num a) => Point a
origin = point 0 0 0

(.-.) :: (Num a) => Point a -> Point a -> Vector a
p .-. q = vector (p^.x - q^.x) (p^.y - q^.y) (p^.z - q^.z)

(.+^), (.-^) :: (Num a) => Point a -> Vector a -> Point a
p .+^ v = point (p^.x + v^.dx) (p^.y + v^.dy) (p^.z + v^.dz)
p .-^ v = point (p^.x - v^.dx) (p^.y + v^.dy) (p^.z + v^.dz)

distance   p q = magnitude   (p .-. q)
distanceSq p q = magnitudeSq (p .-. q)
