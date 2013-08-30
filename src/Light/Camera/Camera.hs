{-# LANGUAGE ExistentialQuantification #-}

module Light.Camera.Camera
  ( Camera(..), CameraBox, cameraBox
  )
where

import Control.Lens

import Light.Camera.Film
import Light.Geometry.Transform
import Light.Geometry.Ray

class Camera a where
  cameraTransform :: Lens' a Transform
  cameraFilm :: Lens' a Film
  cameraRay :: a -> (Double, Double) -> Ray

data CameraBox = forall c. (Camera c, Show c) => CameraBox c

cameraBox :: (Camera c, Show c) => c -> CameraBox
cameraBox = CameraBox

instance Show CameraBox where
  show (CameraBox c) = show c

instance Camera CameraBox where
  cameraTransform = lens (\ (CameraBox c)   -> c^.cameraTransform)
                         (\ (CameraBox c) t -> CameraBox $ (cameraTransform .~ t) c)
  cameraFilm = lens (\ (CameraBox c)   -> c^.cameraFilm)
                    (\ (CameraBox c) f -> CameraBox $ (cameraFilm .~ f) c)
  cameraRay (CameraBox c) = cameraRay c
