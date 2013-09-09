module Light.Camera.PerspectiveCamera
  ( PerspectiveCamera, perspectiveCamera, perspectiveVerticalFOV
  )
where

import Light.Camera
import Light.Film
import Light.Geometry

data PerspectiveCamera = PerspectiveCamera { perspectiveTransform   :: Transform
                                           , perspectiveFilm        :: Film
                                           , perspectiveVerticalFOV :: Double
                                           }
                       deriving (Show)

perspectiveCamera :: Film -> Double -> PerspectiveCamera
perspectiveCamera = PerspectiveCamera identityTransform

instance Camera PerspectiveCamera where
  cameraTransform = perspectiveTransform
  cameraFilm = perspectiveFilm
  cameraRay (PerspectiveCamera t f fovY) (fx, fy) = transform (inverse t) (Ray o d)
    where o  = originPoint
          d  = normalizeV $ Vector (x*sx) (y*sy) 1
          sx = tan (fovY / 2) * (fromIntegral fw / fromIntegral fh)
          sy = tan (fovY / 2)
          x  = (fx / fromIntegral fw) * 2 - 1
          y  = (fy / fromIntegral fh) * 2 - 1
          (fw, fh) = filmDimensions f 

instance Transformable PerspectiveCamera where
  transform t' (PerspectiveCamera t f v) = PerspectiveCamera (compose t' t) f v
