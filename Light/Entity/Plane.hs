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
