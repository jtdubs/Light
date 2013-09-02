module Light.Renderer
  ( render
  )
where

import Codec.Picture
import Control.Lens
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Lazy as BS

import Light.Scene
import Light.Camera.Film
import Light.Camera.Camera
import Light.Geometry.Vector
import Light.Geometry.Point
import Light.Geometry.Ray

render :: Scene -> IO ()
render s = BS.writeFile "out.png" image 
  where
    camera = s^.sceneCamera
    film   = camera^.cameraFilm
    fw     = film^.filmWidth
    fh     = film^.filmHeight
    pixels = [(x, y) | y <- [fh-1,fh-2..0], x <- [0..fw-1]]
    image  = encodePng $ (Image fw fh (V.fromList $ map renderPixel pixels) :: Image Pixel8)

    renderPixel (x, y) = let r = cameraRay camera (fromIntegral x, fromIntegral y)
                         in case sceneIntersect r s of
                              (Just t) -> max 0 (round (255 - 120*((pz $ r `atTime` t) - 9))) :: Pixel8
                              Nothing  -> 0                                                  :: Pixel8
