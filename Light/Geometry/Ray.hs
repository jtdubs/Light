{-# LANGUAGE TemplateHaskell #-}

module Light.Geometry.Ray
    -- ADT
    ( Ray, ray, origin, direction

    -- Default Instances
    , xAxis, yAxis, zAxis

    -- Arithmetic
    , atTime, negateRay
    )
where

import Control.Lens          ((%~), (^.))
import Control.Lens.TH       (makeLenses)
import Light.Geometry.Point  (Point, originPoint, (.+^))
import Light.Geometry.Vector (Vector, unitXVector, unitYVector, unitZVector, negateVector, (^*))

data Ray = Ray { _origin :: Point, _direction :: Vector } deriving (Eq)

ray = Ray

makeLenses ''Ray

instance Show Ray where
  show (Ray o d) = concat ["#R(", show o, ", ", show d, ")"]

xAxis = ray originPoint unitXVector
yAxis = ray originPoint unitYVector
zAxis = ray originPoint unitZVector

atTime r t = (r^.origin) .+^ ((r^.direction) ^* t)

negateRay = direction %~ negateVector
