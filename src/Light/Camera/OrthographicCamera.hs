module Light.Camera.OrthographicCamera
  ( OrthographicCamera, orthographicCamera, orthoScale
  )
where

import Light.Camera
import Light.Film
import Light.Geometry

data OrthographicCamera = OrthographicCamera { orthoTransform :: Transform
                                             , orthoFilm      :: Film
                                             , orthoScale     :: Double
                                             }
                        deriving (Eq, Show, Read)

orthographicCamera :: Film -> Double -> OrthographicCamera
orthographicCamera = OrthographicCamera identityTransform

instance Camera OrthographicCamera where
  cameraTransform = orthoTransform
  cameraFilm = orthoFilm
  cameraRay (OrthographicCamera t f s) (fx, fy) = transform (inverse t) (Ray o d)
    where o = Point (x*s) (y*s) 0
          d = unitZVector
          x = fx - (fromIntegral (filmWidth f)  / 2)
          y = fy - (fromIntegral (filmHeight f) / 2)

instance Transformable OrthographicCamera where
  transform t' (OrthographicCamera t f s) = OrthographicCamera (compose t' t) f s
