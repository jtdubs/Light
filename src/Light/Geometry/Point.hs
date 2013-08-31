module Light.Geometry.Point
  -- ADT
  ( Point(..)

  -- Default Instances
  , originPoint

  -- Arithmetic
  , (.-.), (.+^), (.-^), distance, distanceSquared
  )
where

import Light.Geometry.Vector

data Point = Point { px :: !Double, py :: !Double, pz :: !Double } deriving (Show, Read)

instance Eq Point where
  u == v = distanceSquared u v < 0.000001

originPoint :: Point
originPoint = Point 0 0 0

(.-.) :: Point -> Point -> Vector
(Point x y z) .-. (Point  a b c) = Vector (x-a) (y-b) (z-c)

(.+^), (.-^) :: Point -> Vector -> Point
(Point x y z) .-^ (Vector dx dy dz) = Point (x - dx) (y - dy) (z - dz)
(Point x y z) .+^ (Vector dx dy dz) = Point (x + dx) (y + dy) (z + dz)

infixl 6 .-., .+^, .-^

distance, distanceSquared :: Point -> Point -> Double
distance        p q = magnitudeV        (p .-. q)
distanceSquared p q = magnitudeSquaredV (p .-. q)
