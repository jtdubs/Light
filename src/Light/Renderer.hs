module Light.Renderer
  ( render
  )
where

import Codec.Picture
import Control.Monad
import Data.Random

import Light.Cameras
import Light.Film
import Light.Geometry
import Light.Sampler
import Light.Scene
import Light.Shapes

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = liftM concat (mapM f l)

render :: Scene -> RVar Film
render scene = do
  samples <- concatMapM samplePixel pixels
  foldM (\f s -> return $ addSample f s (renderSample s)) film samples

  where camera   = sceneCamera scene
        film     = cameraFilm camera
        (fw, fh) = filmDimensions film
        pixels   = [(x, y) | x <- [0..fw-1], y <- [0..fh-1]]
        bounds   = foldl aabbUnion EmptyAABB (map worldBound $ scenePrimitives scene)
        minZ     = pz $ aabbMin bounds
        maxZ     = pz $ aabbMax bounds
        pToC p   = min 255 $ max 0 $ round $ 255 - (255/(maxZ-minZ))*(pz p - minZ)

        samplePixel :: (Int, Int) -> RVar [(Double, Double)]
        samplePixel (x, y) = do
          os <- sample022D (fromIntegral x) (fromIntegral y) 8
          return $ map (\ (dx, dy) -> ((fromIntegral x+dx, fromIntegral y+dy))) os

        renderSample :: (Double, Double) -> Pixel8
        renderSample (x, y) = let r = cameraRay camera (x, y)
                              in case sceneIntersect r scene of
                                   (Just t) -> pToC (r `atTime` t) :: Pixel8
                                   Nothing  -> 0                   :: Pixel8
