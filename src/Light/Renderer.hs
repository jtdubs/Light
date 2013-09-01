module Light.Renderer
  ( render
  )
where

import Codec.Picture
import Control.Lens
import Control.Parallel.Strategies
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Lazy as BS

import Light.Scene
import Light.Camera.Film
import Light.Camera.Camera

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
                         in if sceneIntersects r s
                              then 255 :: Pixel8
                              else 0   :: Pixel8
