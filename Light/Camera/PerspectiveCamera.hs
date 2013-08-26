{-# LANGUAGE TemplateHaskell #-}

module Light.Camera.PerspectiveCamera
  ( PerspectiveCamera, perspectiveCamera, perspectiveTransform, perspectiveFilm, perspectiveVerticalFOV
  )
where

import Control.Lens hiding (transform)

import Light.Camera.Film
import Light.Camera.Camera
import Light.Geometry.Transform
import Light.Geometry.Ray
import Light.Geometry.Point
import Light.Geometry.Vector

data PerspectiveCamera = PerspectiveCamera
                          { _perspectiveTransform   :: Transform
                          , _perspectiveFilm        :: Film
                          , _perspectiveVerticalFOV :: Float
                          } deriving (Eq)

makeLenses ''PerspectiveCamera

perspectiveCamera = PerspectiveCamera identityTransform

instance Show PerspectiveCamera where
  show (PerspectiveCamera t f s) = concat ["#O(", show t, ", ", show f, ", ", show s, ")"]

instance Camera PerspectiveCamera where
  cameraTransform = perspectiveTransform
  cameraFilm = perspectiveFilm
  cameraRay (PerspectiveCamera t (Film fw fh) fovY) (fx, fy) = transform (inverse t) (ray o d)
    where o = originPoint
          d = normalizeV $ vector (nx*sx) (ny*sy) 1
          sx = atan (fovY / 2)
          sy = atan (fovY * (fromIntegral fh / fromIntegral fw) / 2)
          nx = (fromIntegral fx / fromIntegral (fw-1)) - 0.5
          ny = (fromIntegral fy / fromIntegral (fh-1)) - 0.5
