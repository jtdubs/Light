module Light.Camera.Camera
  ( Camera(..)
  )
where

import Control.Lens

import Light.Camera.Film
import Light.Geometry.Transform
import Light.Geometry.Ray

class Camera a where
  cameraTransform :: Lens' a Transform
  cameraFilm :: Lens' a Film
  cameraRay :: a -> (Float, Float) -> Ray
