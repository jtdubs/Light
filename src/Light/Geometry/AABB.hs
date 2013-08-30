{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.AABB
  -- ADT
  ( AABB, aabb, emptyAABB, isEmpty, aabbMin, aabbMax, fromPoint, fromPoints

  -- Arithmetic
  , addPoint, addPoints, aabbUnion, overlaps, isInside, aabbSurfaceArea, volume, corners
  )
where

import Control.Lens

import Light.Geometry.Point
import Light.Geometry.Vector

data AABB = EmptyAABB | AABB { _aabbMin :: Point, _aabbMax :: Point } deriving (Eq, Show, Read)

aabb :: Point -> Point -> AABB
aabb = AABB

emptyAABB :: AABB
emptyAABB = EmptyAABB

isEmpty :: AABB -> Bool
isEmpty EmptyAABB = True
isEmpty _         = False

makeLenses ''AABB

fromPoint :: Point -> AABB
fromPoint  = addPoint  EmptyAABB

fromPoints :: [Point] -> AABB
fromPoints = addPoints EmptyAABB

addPoint :: AABB -> Point -> AABB
addPoint EmptyAABB  p = AABB p p
addPoint (AABB n x) p = AABB (point (min (n^.px) (p^.px)) (min (n^.py) (p^.py)) (min (n^.pz) (p^.pz)))
                             (point (max (x^.px) (p^.px)) (max (x^.py) (p^.py)) (max (x^.pz) (p^.pz)))

addPoints :: AABB -> [Point] -> AABB
addPoints = foldl addPoint

aabbUnion :: AABB -> AABB -> AABB
aabbUnion EmptyAABB b = b
aabbUnion b EmptyAABB = b
aabbUnion (AABB n x) (AABB o y) = AABB (point (min (n^.px) (o^.px)) (min (n^.py) (o^.py)) (min (n^.pz) (o^.pz)))
                                       (point (max (x^.px) (y^.px)) (max (x^.py) (y^.py)) (max (x^.pz) (y^.pz)))

overlaps :: AABB -> AABB -> Bool
overlaps _ EmptyAABB  = False
overlaps EmptyAABB _  = False
overlaps b (AABB n x) = isInside b n || isInside b x

isInside :: AABB -> Point -> Bool
isInside EmptyAABB  _ = False
isInside (AABB n x) p = ((p^.px) >= (n^.px) && (p^.px) <= (x^.px))
                     && ((p^.py) >= (n^.py) && (p^.py) <= (x^.py))
                     && ((p^.pz) >= (n^.pz) && (p^.pz) <= (x^.pz))

aabbSurfaceArea :: AABB -> Double
aabbSurfaceArea EmptyAABB = 0
aabbSurfaceArea (AABB n x) = 2 * ((d^.dx * d^.dy) + (d^.dx * d^.dz) + (d^.dy * d^.dz))
  where d = x .-. n

volume :: AABB -> Double
volume EmptyAABB = 0
volume (AABB x n) = d^.dx * d^.dy * d^.dz
  where d = x .-. n

corners :: AABB -> [Point]
corners EmptyAABB = []
corners (AABB n x) = [ point nx ny nz, point nx ny xz, point nx xy nz, point nx xy xz
                     , point xx ny nz, point xx ny xz, point xx xy nz, point xx xy xz ]
  where nx=n^.px; ny=n^.py; nz=n^.pz
        xx=x^.px; xy=x^.py; xz=x^.pz

-- TODO: convert to bounding sphere
