module Light.Renderer
  ( render
  )
where

import Codec.Picture
import Control.Lens
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Lazy as BS

import Light.Scene
import Light.Shape
import Light.Camera
import Light.Geometry

render :: Scene -> IO ()
render s = BS.writeFile "out.png" image 
  where
    camera = s^.sceneCamera
    film   = camera^.cameraFilm
    fw     = film^.filmWidth
    fh     = film^.filmHeight
    pixels = [(x, y) | y <- [fh-1,fh-2..0], x <- [0..fw-1]]
    image  = encodePng $ (Image fw fh (V.fromList $ map renderPixel pixels) :: Image Pixel8)
    bounds = foldl aabbUnion EmptyAABB (map worldBound $ s^.scenePrimitives)
    minZ   = pz $ aabbMin bounds
    maxZ   = pz $ aabbMax bounds
    pToC p = min 255 $ max 0 $ round $ 255 - (255/(maxZ-minZ))*(pz p - minZ)

    renderPixel (x, y) = let r = cameraRay camera (fromIntegral x, fromIntegral y)
                         in case sceneIntersect r s of
                              (Just t) -> pToC (r `atTime` t) :: Pixel8
                              Nothing  -> 0                   :: Pixel8
