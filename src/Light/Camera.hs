{-# LANGUAGE ExistentialQuantification #-}

module Light.Camera
  ( Camera(..), CameraBox, cameraBox
  )
where

import Light.Film
import Light.Geometry

class Camera a where
  cameraTransform :: a -> Transform
  cameraFilm      :: a -> Film
  cameraRay       :: a -> (Double, Double) -> Ray

data CameraBox = forall c. (Camera c, Transformable c, Show c) => CameraBox c

cameraBox :: (Camera c, Transformable c, Show c) => c -> CameraBox
cameraBox = CameraBox

instance Show CameraBox where
  show (CameraBox c) = show c

instance Camera CameraBox where
  cameraTransform (CameraBox c) = cameraTransform c
  cameraFilm (CameraBox c) = cameraFilm c
  cameraRay (CameraBox c) = cameraRay c

instance Transformable CameraBox where
  transform t' (CameraBox c) = CameraBox (transform t' c)
