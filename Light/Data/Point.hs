{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.Point
    -- ADT
	( Point, point, x, y, z, ps, toList, fromList

	-- Default Instances
    , origin

	-- Arithmetic
	, (.-.), (.+^), (.-^), distance, distanceSq
	)
where

import Control.Lens      ((^.), Lens', lens)
import Control.Lens.TH   (makeLenses)
import Light.Data.Vector (vector, dx, dy, dz, magnitude, magnitudeSq)

data Point = Point { _x :: Float, _y :: Float, _z :: Float }

point = Point

makeLenses ''Point

toList (Point x y z) = [x, y, z, 1]

fromList [x, y, z] = Point x y z
fromList [x, y, z, w] = Point (x/w) (y/w) (z/w)

ps :: Lens' Point [Float]
ps = lens toList (\p l -> fromList l)

instance Eq Point where
  u == v = distanceSq u v < 0.00001

instance Show Point where
  show (Point x y z) = concat ["#P(", show x, ", ", show y, ", ", show z, ")"]

origin = point 0 0 0

pv op (Point x y z) v = point  (op x (v^.dx)) (op y (v^.dy)) (op z (v^.dz))
pp op (Point a b c) p = vector (op a (p^.x))  (op b (p^.y))  (op c (p^.z))

(.-.) = pp (-)
(.+^) = pv (+)
(.-^) = pv (-)

distance   p q = magnitude   (p .-. q)
distanceSq p q = magnitudeSq (p .-. q)
