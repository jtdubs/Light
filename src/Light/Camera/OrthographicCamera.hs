{-# LANGUAGE TemplateHaskell #-}

module Light.Camera.OrthographicCamera
  ( OrthographicCamera, orthographicCamera, orthoScale
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
                          , _orthoScale     :: Double
                          } deriving (Eq, Show, Read)

makeLenses ''OrthographicCamera

orthographicCamera :: Film -> Double -> OrthographicCamera
orthographicCamera = OrthographicCamera identityTransform

instance Camera OrthographicCamera where
  cameraTransform = orthoTransform
  cameraFilm = orthoFilm
  cameraRay (OrthographicCamera t f s) (fx, fy) = transform (inverse t) (Ray o d)
    where o = Point (x*s) (y*s) 0
          d = unitZVector
          x = fx - (fromIntegral (f^.filmWidth)  / 2)
          y = fy - (fromIntegral (f^.filmHeight) / 2)
