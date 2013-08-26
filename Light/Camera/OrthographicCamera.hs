{-# LANGUAGE TemplateHaskell #-}

module Light.Camera.OrthographicCamera
  ( OrthographicCamera, orthographicCamera, orthoTransform, orthoFilm, orthoSize
  , unitOrthographicCamera
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
                          , _orthoSize      :: (Float, Float)
                          } deriving (Eq)

makeLenses ''OrthographicCamera

orthographicCamera = OrthographicCamera identityTransform

unitOrthographicCamera = orthographicCamera (Film 320 240) (3, 2)

instance Show OrthographicCamera where
  show (OrthographicCamera t f s) = concat ["#O(", show t, ", ", show f, ", ", show s, ")"]

instance Camera OrthographicCamera where
  cameraTransform = orthoTransform
  cameraFilm = orthoFilm
  cameraRay (OrthographicCamera t (Film fw fh) (w, h)) (x, y) = transform (inverse t) (ray o d)
    where o = point (x'*w/fw' - w/2) (y'*h/fh' - h/2) 0
          d = unitZVector
          x' = fromIntegral x
          y' = fromIntegral y
          fw' = fromIntegral fw
          fh' = fromIntegral fh
