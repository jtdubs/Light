{-# LANGUAGE TemplateHaskell #-}

module Light.Geometry.Ray
  -- ADT
  ( Ray, ray, rayOrigin, rayDirection

  -- Default Instances
  , xAxisRay, yAxisRay, zAxisRay

  -- Arithmetic
  , atTime, negateRay
  )
where

import Control.Lens
import Control.Lens.TH

import Light.Geometry.Point
import Light.Geometry.Vector

data Ray = Ray { _rayOrigin :: Point, _rayDirection :: Vector } deriving (Eq)

ray = Ray

makeLenses ''Ray

instance Show Ray where
  show (Ray o d) = concat ["#R(", show o, ", ", show d, ")"]

xAxisRay = Ray originPoint unitXVector
yAxisRay = Ray originPoint unitYVector
zAxisRay = Ray originPoint unitZVector

atTime (Ray o d) t = o .+^ (d ^* t)

negateRay = rayDirection %~ negateVector
