{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.Point
    -- ADT
	( Point, point, x, y, z, ps

	-- Default Instances
    , originPoint

	-- Arithmetic
	, (.-.), (.+^), (.-^), distance, distanceSquared
	)
where

import Control.Lens          ((^.), Lens', lens)
import Control.Lens.TH       (makeLenses)
import Light.Geometry.Vector (vector, dx, dy, dz, magnitude, magnitudeSquared)

data Point = Point { _x :: Float, _y :: Float, _z :: Float }

point = Point

makeLenses ''Point

ps :: Lens' Point [Float]
ps = lens (\ (Point x y z) -> [x, y, z, 1]) (\p [x, y, z, w] -> Point (x/w) (y/w) (z/w))

instance Eq Point where
  u == v = distanceSquared u v < 0.00001

instance Show Point where
  show (Point x y z) = concat ["#P(", show x, ", ", show y, ", ", show z, ")"]

originPoint = point 0 0 0

pv op (Point x y z) v = point  (op x (v^.dx)) (op y (v^.dy)) (op z (v^.dz))
pp op (Point a b c) p = vector (op a (p^.x))  (op b (p^.y))  (op c (p^.z))

(.-.) = pp (-)
(.+^) = pv (+)
(.-^) = pv (-)

distance        p q = magnitude   (p .-. q)
distanceSquared p q = magnitudeSquared (p .-. q)
