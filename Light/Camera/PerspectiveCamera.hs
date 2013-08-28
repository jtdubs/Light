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
          sx = tan (fovY / 2) * (fromIntegral (fw-1) / fromIntegral fw) * (fromIntegral fw / fromIntegral fh)
          sy = tan (fovY / 2) * (fromIntegral (fh-1) / fromIntegral fh)
          nx = (fx / fromIntegral (fw-1)) * 2 - 1
          ny = (fy / fromIntegral (fh-1)) * 2 - 1
