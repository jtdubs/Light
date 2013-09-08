{-# LANGUAGE TemplateHaskell #-}

module Light.Camera.PerspectiveCamera
  ( PerspectiveCamera, perspectiveCamera, perspectiveVerticalFOV
  )
where

import Control.Lens hiding (transform)

import Light.Camera
import Light.Camera.Film
import Light.Geometry

data PerspectiveCamera = PerspectiveCamera
                          { _perspectiveTransform   :: Transform
                          , _perspectiveFilm        :: Film
                          , _perspectiveVerticalFOV :: Double
                          } deriving (Eq, Show, Read)

makeLenses ''PerspectiveCamera

perspectiveCamera :: Film -> Double -> PerspectiveCamera
perspectiveCamera = PerspectiveCamera identityTransform

instance Camera PerspectiveCamera where
  cameraTransform = perspectiveTransform
  cameraFilm = perspectiveFilm
  cameraRay (PerspectiveCamera t f fovY) (fx, fy) = transform (inverse t) (Ray o d)
    where o  = originPoint
          d  = normalizeV $ Vector (x*sx) (y*sy) 1
          fw = f^.filmWidth
          fh = f^.filmHeight
          sx = tan (fovY / 2) * (fromIntegral fw / fromIntegral fh)
          sy = tan (fovY / 2)
          x  = (fx / fromIntegral fw) * 2 - 1
          y  = (fy / fromIntegral fh) * 2 - 1
