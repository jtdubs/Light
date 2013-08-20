{-# LANGUAGE TemplateHaskell #-}

module Light.Data.Ray
    -- ADT
    ( Ray, ray, origin, direction

    -- Default Instances
    , xAxis, yAxis, zAxis

    -- Arithmetic
    , negateR
    )
where

import Control.Lens.TH             (makeLenses)
import Light.Data.Point            (Point)
import Light.Data.Vector           (Vector, unitX, unitY, unitZ, negateV)
import Light.Data.CoordinateSystem (Orientable(..))

import qualified Light.Data.Point as P

data Ray = Ray { _origin :: Point, _direction :: Vector }

ray = Ray

makeLenses ''Ray

instance Eq Ray where
  (Ray o d) == (Ray p e) = o == p && d == e

instance Show Ray where
  show (Ray o d) = concat ["#R(", show o, ", ", show d, ")"]

instance Orientable Ray where
  toGlobal c (Ray o d) = Ray (toGlobal c o) (toGlobal c d)
  toLocal  c (Ray o d) = Ray (toLocal  c o) (toLocal  c d)

xAxis = ray P.origin unitX
yAxis = ray P.origin unitY
zAxis = ray P.origin unitZ

negateR (Ray o d) = Ray o (negateV d)
