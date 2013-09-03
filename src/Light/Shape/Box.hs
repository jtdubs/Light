{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Box
  -- ADT
  ( Box, box, boxHalfWidth, boxHalfDepth, boxHalfHeight

  -- Default Instances
  , unitBox
  )
where

import Control.Monad
import Control.Lens hiding (transform)
import Data.List

import Light.Geometry.AABB
import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Shape.Shape

data Box = Box { _boxTransform    :: Transform
               , _boxHalfWidth    :: Double
               , _boxHalfHeight   :: Double
               , _boxHalfDepth    :: Double
               } deriving (Show, Read)

box :: Double -> Double -> Double -> Box
box = Box identityTransform

makeLenses ''Box

unitBox :: Box
unitBox = box 1 1 1

instance Transformable Box where
  transform t' (Box t w h d) = Box (compose t' t) w h d

instance Shape Box where
  shapeTransform = boxTransform

  bound (Box _ w h d) = fromPoints [ Point (-w) (-h) (-d), Point w h d ]

  surfaceArea (Box _ w h d) = (8*d*w) + (8*d*h) + (8*w*h) 

  intersections theRay (Box t w h d) = do
    guard $ t0x < t1y && t0x < t1z
    guard $ t0y < t1z && t0y < t1x
    guard $ t0z < t1x && t0z < t1y
    guard $ not (null ts)
    ts
    where r'         = transform (inverse t) theRay
          rdx        = dx $ rayDirection r'
          rdy        = dy $ rayDirection r'
          rdz        = dz $ rayDirection r'
          rox        = px $ rayOrigin r'
          roy        = py $ rayOrigin r'
          roz        = pz $ rayOrigin r'
          [t0x, t1x] = sort [ ((-w) - rox) / rdx , (  w  - rox) / rdx ]
          [t0y, t1y] = sort [ ((-h) - roy) / rdy , (  h  - roy) / rdy ]
          [t0z, t1z] = sort [ ((-d) - roz) / rdz , (  d  - roz) / rdz ]
          t0         = maximum [ t0x, t0y, t0z ]
          t1         = minimum [ t1x, t1y, t1z ]
          ts         = filter (> 0) [ t0, t1 ]
