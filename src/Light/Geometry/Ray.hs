{-# LANGUAGE TemplateHaskell #-}

module Light.Geometry.Ray
  -- ADT
  ( Ray, ray, rayOrigin, rayDirection

  -- Default Instances
  , xAxisRay, yAxisRay, zAxisRay

  -- Arithmetic
  , atTime, negateR
  )
where

import Control.Lens

import Light.Geometry.Point
import Light.Geometry.Vector

data Ray = Ray { _rayOrigin :: Point, _rayDirection :: Vector } deriving (Eq, Show, Read)

ray :: Point -> Vector -> Ray
ray = Ray

makeLenses ''Ray

xAxisRay, yAxisRay, zAxisRay :: Ray
xAxisRay = Ray originPoint unitXVector
yAxisRay = Ray originPoint unitYVector
zAxisRay = Ray originPoint unitZVector

atTime :: Ray -> Double -> Point
atTime (Ray o d) t = o .+^ (d ^* t)

negateR :: Ray -> Ray
negateR = rayDirection %~ negateV
