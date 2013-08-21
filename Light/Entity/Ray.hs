{-# LANGUAGE TemplateHaskell #-}

module Light.Entity.Ray
    -- ADT
    ( Ray, ray, origin, direction

    -- Default Instances
    , xAxis, yAxis, zAxis

    -- Arithmetic
    , negateR
    )
where

import Control.Lens      (lens, (%~))
import Control.Lens.TH   (makeLenses)
import Light.Math.Point  (Point, originPoint)
import Light.Math.Vector (Vector, unitXVector, unitYVector, unitZVector, negateV)
import Light.Math.Basis  (Basis, defaultBasis, HasBasis(..), Orientable(..))

data Ray = Ray { _rayBasis:: Basis, _origin :: Point, _direction :: Vector } deriving (Eq)

ray = Ray defaultBasis

makeLenses ''Ray

instance Show Ray where
  show (Ray b o d) = concat ["#R(", show b, ", ", show o, ", ", show d, ")"]

instance HasBasis Ray where
  basis = rayBasis

instance Orientable Ray where
  outOf c = basis %~ outOf c 
  into  c = basis %~ into  c

xAxis = ray originPoint unitXVector
yAxis = ray originPoint unitYVector
zAxis = ray originPoint unitZVector

negateR = direction %~ negateV
