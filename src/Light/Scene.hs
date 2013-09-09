module Light.Scene
  ( Scene, scene, sceneCamera, scenePrimitives
  , sceneIntersects, sceneIntersect
  )
where

import Data.Maybe

import Light.Camera
import Light.Shape
import Light.Primitive
import Light.Geometry

data Scene = Scene { sceneCamera     :: CameraBox
                   , scenePrimitives :: [Primitive]
                   }
           deriving (Show)

scene :: (Camera c, Transformable c, Show c) => c -> [Primitive] -> Scene
scene c = Scene (cameraBox c)

sceneIntersects :: Ray -> Scene -> Bool
sceneIntersects r (Scene _ ps) = any (intersects r) ps

sceneIntersect :: Ray -> Scene -> Maybe Double
sceneIntersect r (Scene _ ps) = case mapMaybe (intersect r) ps
                                of [] -> Nothing
                                   ts -> Just $ minimum ts
