{-# LANGUAGE TemplateHaskell #-}

module Light.Camera.OrthographicCamera
  ( OrthographicCamera, orthographicCamera, orthoTransform, orthoFilm, orthoScale
  )
where

import Control.Lens hiding (transform)

import Light.Camera.Film
import Light.Camera.Camera
import Light.Geometry.Transform
import Light.Geometry.Ray
import Light.Geometry.Point
import Light.Geometry.Vector

data OrthographicCamera = OrthographicCamera
                          { _orthoTransform :: Transform
                          , _orthoFilm      :: Film
                          , _orthoScale     :: Float
                          } deriving (Eq)

makeLenses ''OrthographicCamera

orthographicCamera = OrthographicCamera identityTransform

instance Show OrthographicCamera where
  show (OrthographicCamera t f s) = concat ["#O(", show t, ", ", show f, ", ", show s, ")"]

instance Camera OrthographicCamera where
  cameraTransform = orthoTransform
  cameraFilm = orthoFilm
  cameraRay (OrthographicCamera t (Film fw fh) s) (fx, fy) = transform (inverse t) (ray o d)
    where o = point (x*s) (y*s) 0
          d = unitZVector
          x = fx - ((fromIntegral (fw-1)) / 2)
          y = fy - ((fromIntegral (fh-1)) / 2)
