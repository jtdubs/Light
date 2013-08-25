{-# LANGUAGE TemplateHaskell #-}

module Light.Entity.Plane
    -- ADT
    ( Plane, plane, normal, distance

    -- Default Instances
    , xyPlane, xzPlane, yzPlane
    )
where

import Control.Lens      ((%~))
import Control.Lens.TH   (makeLenses)
import Light.Entity.Ray  (Ray, origin, direction, RayTestable(..))
import Light.Math.Vector (Vector, unitXVector, unitYVector, unitZVector)
import Light.Math.Basis  (defaultBasis, Basis, HasBasis(..), Orientable(..))

data Plane = Plane { _planeBasis :: Basis, _normal :: Vector, _distance :: Float } deriving (Eq)

plane = Plane defaultBasis

makeLenses ''Plane

instance Show Plane where
  show (Plane b n d) = concat ["#P(", show b, ", ", show n, ", ", show d, ")"]

instance HasBasis Plane where
  basis = planeBasis

instance Orientable Plane where
  outOf c = basis %~ outOf c 
  into  c = basis %~ into  c

xyPlane = plane unitZVector 0
xzPlane = plane unitYVector 0
yzPlane = plane unitXVector 0

instance RayTestable Plane where
  rayTest r p = if d == 0 || t < 0
                  then Nothing
                  else Just (t, if side < 0 then Back else Front, r `atTime` t)
    where n = (p^.normal ^.^ (r^.origin .-. originPoint)) + p^.distance
          d = p^.normal ^.^ r^.direction
          t = -n/d
          side = p^.normal ^.^ (r^.origin .-. originPoint)

  rayTest :: Ray -> a -> Maybe (Float, SurfaceType, Point)
