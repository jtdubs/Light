{-# LANGUAGE TypeFamilies #-}

module Light.Geometry.AABB
  -- ADT
  ( AABB(..), isEmpty, fromPoint, fromPoints

  -- Arithmetic
  , addPoint, addPoints, aabbUnion, overlaps, isInside, aabbSurfaceArea, volume, corners
  )
where

import Light.Geometry.Point
import Light.Geometry.Vector

data AABB = EmptyAABB
          | AABB { aabbMin :: !Point
                 , aabbMax :: !Point
                 }
          deriving (Eq, Show, Read)

isEmpty :: AABB -> Bool
isEmpty EmptyAABB = True
isEmpty _         = False

fromPoint :: Point -> AABB
fromPoint  = addPoint  EmptyAABB

fromPoints :: [Point] -> AABB
fromPoints = addPoints EmptyAABB

addPoint :: AABB -> Point -> AABB
addPoint EmptyAABB  p = AABB p p
addPoint (AABB n x) p = AABB (Point (min (px n) (px p)) (min (py n) (py p)) (min (pz n) (pz p)))
                             (Point (max (px x) (px p)) (max (py x) (py p)) (max (pz x) (pz p)))

addPoints :: AABB -> [Point] -> AABB
addPoints = foldl addPoint

aabbUnion :: AABB -> AABB -> AABB
aabbUnion EmptyAABB b = b
aabbUnion b EmptyAABB = b
aabbUnion (AABB n x) (AABB o y) = AABB (Point (min (px n) (px o)) (min (py n) (py o)) (min (pz n) (pz o)))
                                       (Point (max (px x) (px y)) (max (py x) (py y)) (max (pz x) (pz y)))

overlaps :: AABB -> AABB -> Bool
overlaps _ EmptyAABB  = False
overlaps EmptyAABB _  = False
overlaps b (AABB n x) = isInside b n || isInside b x

isInside :: AABB -> Point -> Bool
isInside EmptyAABB  _ = False
isInside (AABB n x) p = px p >= px n && px p <= px x
                     && py p >= py n && py p <= py x
                     && pz p >= pz n && pz p <= pz x

aabbSurfaceArea :: AABB -> Double
aabbSurfaceArea EmptyAABB = 0
aabbSurfaceArea (AABB n x) = 2 * ((dx d * dy d) + (dx d * dz d) + (dy d * dz d))
  where d = x .-. n

volume :: AABB -> Double
volume EmptyAABB = 0
volume (AABB x n) = dx d * dy d * dz d
  where d = x .-. n

corners :: AABB -> [Point]
corners EmptyAABB = []
corners (AABB n x) = [ Point nx ny nz, Point nx ny xz, Point nx xy nz, Point nx xy xz
                     , Point xx ny nz, Point xx ny xz, Point xx xy nz, Point xx xy xz ]
  where nx=px n; ny=py n; nz=pz n
        xx=px x; xy=py x; xz=pz x

-- TODO: convert to bounding sphere
