{-# LANGUAGE TemplateHaskell #-}

module Light.Data.Plane
    -- ADT
    ( Plane, plane, normal, distance

    -- Default Instances
    , xyPlane, xzPlane, yzPlane
    )
where

import Control.Lens.TH             (makeLenses)
import Light.Data.Vector           (Vector, unitX, unitY, unitZ, (^*))
import Light.Data.Point            (origin, (.+^))
import Light.Data.CoordinateSystem (Orientable(..))

import qualified Light.Data.Point as P

data Plane = Plane { _normal :: Vector, _distance :: Float }

plane = Plane

makeLenses ''Plane

instance Eq Plane where
  (Plane n d) == (Plane m e) = n == m && d == e

instance Show Plane where
  show (Plane n d) = concat ["#P(", show n, ", ", show d, ")"]

instance Orientable Plane where
  toGlobal c (Plane n d) = Plane (toGlobal c n) (P.distance origin (toGlobal c (origin .+^ (n ^* d))))
  toLocal  c (Plane n d) = Plane (toLocal  c n) (P.distance origin (toLocal  c (origin .+^ (n ^* d))))

xyPlane = plane unitZ 0
xzPlane = plane unitY 0
yzPlane = plane unitX 0
