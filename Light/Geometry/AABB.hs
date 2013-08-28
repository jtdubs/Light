{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.AABB
	-- ADT
	( AABB, aabb, emptyAABB, isEmpty, aabbMin, aabbMax, fromPoint, fromPoints

	-- Arithmetic
	, addPoint, addPoints, aabbUnion, overlaps, isInside, surfaceArea, volume, corners
	)
where

import Control.Lens
import Control.Lens.TH

import Light.Geometry.Point
import Light.Geometry.Vector

data AABB = EmptyAABB | AABB { _aabbMin :: Point, _aabbMax :: Point } deriving (Eq)

aabb = AABB
emptyAABB = EmptyAABB

isEmpty EmptyAABB = True
isEmpty _         = False

makeLenses ''AABB

instance Show AABB where
  show EmptyAABB  = "#AABB()"
  show (AABB n x) = "#AABB(" ++ show n ++ ", " ++ show x ++ ")"

fromPoint  = addPoint  EmptyAABB
fromPoints = addPoints EmptyAABB

addPoint EmptyAABB  p = AABB p p
addPoint (AABB n x) p = AABB (point (min (n^.px) (p^.px)) (min (n^.py) (p^.py)) (min (n^.pz) (p^.pz)))
                             (point (max (n^.px) (p^.px)) (max (n^.py) (p^.py)) (max (n^.pz) (p^.pz)))

addPoints = foldl addPoint

aabbUnion EmptyAABB b = b
aabbUnion b EmptyAABB = b
aabbUnion (AABB n x) (AABB o y) = AABB (point (min (n^.px) (o^.px)) (min (n^.py) (o^.py)) (min (n^.pz) (o^.pz)))
                                       (point (max (x^.px) (y^.px)) (max (x^.py) (y^.py)) (max (x^.pz) (y^.pz)))

overlaps b EmptyAABB  = False
overlaps EmptyAABB b  = False
overlaps b (AABB n x) = isInside b n || isInside b x

isInside EmptyAABB  p = False
isInside (AABB n x) p = ((p^.px) >= (n^.px) && (p^.px) <= (x^.px))
                   && ((p^.py) >= (n^.py) && (p^.py) <= (x^.py))
                   && ((p^.pz) >= (n^.pz) && (p^.pz) <= (x^.pz))

surfaceArea EmptyAABB = 0
surfaceArea (AABB n x) = 2 * ((d^.dx * d^.dy) + (d^.dx * d^.dz) + (d^.dy * d^.dz))
  where d = x .-. n

volume EmptyAABB = 0
volume (AABB x n) = d^.dx * d^.dy * d^.dz
  where d = x .-. n

corners EmptyAABB = []
corners (AABB n x) = [ point nx ny nz, point nx ny xz, point nx xy nz, point nx xy xz
                     , point xx ny nz, point xx ny xz, point xx xy nz, point xx xy xz ]
  where nx=n^.px; ny=n^.py; nz=n^.pz
        xx=x^.px; xy=x^.py; xz=x^.pz

-- TODO: convert to bounding sphere
