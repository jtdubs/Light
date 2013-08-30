{-# LANGUAGE TemplateHaskell #-}

module Light.Scene
  ( Scene, scene, sceneCamera, scenePrimitives
  , sceneIntersects, sceneIntersect
  )
where

import Control.Lens
import Data.Maybe

import Light.Camera.Camera
import Light.Shape.Shape
import Light.Primitive
import Light.Geometry.Ray

data Scene = Scene { _sceneCamera :: CameraBox, _scenePrimitives :: [Primitive] } deriving (Show)

scene :: (Camera c, Show c) => c -> [Primitive] -> Scene
scene c = Scene (cameraBox c)

makeLenses ''Scene

sceneIntersects :: Ray -> Scene -> Bool
sceneIntersects r (Scene _ ps) = any (intersects r) ps

sceneIntersect :: Ray -> Scene -> Maybe Double
sceneIntersect r (Scene _ ps) = case catMaybes $ map (intersect r) ps
                         of [] -> Nothing
                            ts -> Just $ minimum ts
