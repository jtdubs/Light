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

import Light.Geometry.Vector

data Point = Point { _px :: Float, _py :: Float, _pz :: Float } deriving (Show, Read)

point :: Float -> Float -> Float -> Point
point = Point

makeLenses ''Point

ps :: Lens' Point [Float]
ps = lens (\ (Point x y z) -> [x, y, z, 1])
          (\_ [x, y, z, w] -> if w == 0 
                              then Point 0 0 0
                              else Point (x/w) (y/w) (z/w))

instance Eq Point where
  u == v = distanceSquared u v < 0.00001

originPoint :: Point
originPoint = point 0 0 0

(.-.) :: Point -> Point -> Vector
(Point x y z) .-. (Point  a b c) = vector (x-a) (y-b) (z-c)

(.+^), (.-^) :: Point -> Vector -> Point
(Point x y z) .-^ v = Point (x - v^.dx) (y - v^.dy) (z - v^.dz)
(Point x y z) .+^ v = Point (x + v^.dx) (y + v^.dy) (z + v^.dz)

infixl 6 .-., .+^, .-^

distance, distanceSquared :: Point -> Point -> Float
distance        p q = magnitudeV        (p .-. q)
distanceSquared p q = magnitudeSquaredV (p .-. q)
