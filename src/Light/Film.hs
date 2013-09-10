module Light.Film
  ( Film, film, filmDimensions, filmSampleBounds, filmFilter, filmPixels, addSample
  , film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA
  , Pixel, pixelSum, pixelWeightSum
  , toPNG
  )
where

import qualified Codec.Picture as P
import Data.Array
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V

import Light.Filter

data Pixel = Pixel { pixelSum       :: Double
                   , pixelWeightSum :: Double
                   }
           deriving (Show)

data Film = Film { filmDimensions :: (Int, Int)
                 , filmFilter     :: FilterBox
                 , filmPixels     :: Array (Int, Int) Pixel
                 }
          deriving (Show)

film :: (Show f, Filter f) => (Int, Int) -> f -> Film
film b@(w, h) f = Film b (filterBox f) (listArray ((0, 0), (w-1, h-1)) (cycle [Pixel 0 0]))

filmSampleBounds :: Film -> ((Int, Int), (Int, Int))
filmSampleBounds (Film (w, h) f _) = let (ex, ey) = filterExtent f
                                     in ((floor (-ex), floor (-ey)), (ceiling (fromIntegral w+ex), ceiling (fromIntegral h+ey)))

addSample :: Film -> (Double, Double) -> P.Pixel8 -> Film
addSample (Film (fw, fh) f p) (x, y) v = Film (fw, fh) f (p // updates)
  where (ex, ey) = filterExtent f
        (minX, minY) = (max 0      (ceiling (x - 0.5 - ex)), max 0      (ceiling (y - 0.5 - ey)))
        (maxX, maxY) = (min (fw-1) (floor   (x - 0.5 + ex)), min (fh-1) (floor   (y - 0.5 + ey)))
        updates      = [((ux, uy), update ux uy) | ux <- [minX..maxX], uy <- [minY..maxY]]
        update ux uy = let (Pixel s ws) = p ! (ux, uy)
                           pv           = filterWeight f (fromIntegral ux - x - 0.5, fromIntegral uy - y - 0.5)
                       in Pixel (s + fromIntegral v*pv) (ws + pv)

toPNG :: Film -> BS.ByteString
toPNG (Film (w, h) _ p) = P.encodePng (P.Image w h (V.fromList $ map (\ (Pixel s w) -> round (s/w)) [p!(x,y) | y <- [h-1,h-2..0], x <- [0..w-1]]) :: P.Image P.Pixel8)

film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA :: (Filter f, Show f) => f -> Film
film1080      = film (1920, 1080)
film720       = film (1280,  720)
film480       = film ( 720,  480)
film2k        = film (2048, 1080)
film4k        = film (4096, 2160)
film8k        = film (8192, 4608)
filmQVGA      = film ( 320,  240)
filmVGA       = film ( 640,  480)
