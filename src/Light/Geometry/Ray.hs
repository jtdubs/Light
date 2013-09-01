module Light.Geometry.Ray
  -- ADT
  ( Ray(..)

  -- Default Instances
  , xAxisRay, yAxisRay, zAxisRay

  -- Arithmetic
  , atTime, negateR
  )
where

import Light.Geometry.Point
import Light.Geometry.Vector

data Ray = Ray { rayOrigin :: !Point, rayDirection :: !Vector }
         deriving (Eq, Show, Read)

xAxisRay, yAxisRay, zAxisRay :: Ray
xAxisRay = Ray originPoint unitXVector
yAxisRay = Ray originPoint unitYVector
zAxisRay = Ray originPoint unitZVector

atTime :: Ray -> Double -> Point
atTime (Ray o d) t = o .+^ (d ^* t)

negateR :: Ray -> Ray
negateR (Ray o d) = Ray o (negateV d)
