{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.Point
    -- ADT
	( Point, point, px, py, pz, ps

	-- Default Instances
    , originPoint

	-- Arithmetic
	, (.-.), (.+^), (.-^), distance, distanceSquared
	)
where

import Control.Lens
import Control.Lens.TH

import Light.Geometry.Vector

data Point = Point { _px :: Float, _py :: Float, _pz :: Float }

point = Point

makeLenses ''Point

ps :: Lens' Point [Float]
ps = lens (\ (Point x y z) -> [x, y, z, 1]) (\p [x, y, z, w] -> Point (x/w) (y/w) (z/w))

instance Eq Point where
  u == v = distanceSquared u v < 0.00001

instance Show Point where
  show (Point x y z) = concat ["#P(", show x, ", ", show y, ", ", show z, ")"]

originPoint = point 0 0 0

(Point x y z) .-. (Point  a b c) = vector (x-a) (y-b) (z-c)
(Point x y z) .-^ v = Point (x - v^.dx) (y - v^.dy) (z - v^.dz)
(Point x y z) .+^ v = Point (x + v^.dx) (y + v^.dy) (z + v^.dz)

distance        p q = magnitudeV        (p .-. q)
distanceSquared p q = magnitudeSquaredV (p .-. q)
