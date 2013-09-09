module Light.Film
  ( Film, film, filmDimensions, filmSampleBounds, filmFilter
  , film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA
  )
where

import Data.Array
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
film b@(w, h) f = Film b (filterBox f) (array ((0, 0), (w-1, h-1)) [])

filmSampleBounds :: Film -> ((Int, Int), (Int, Int))
filmSampleBounds (Film (w, h) f _) = let (ex, ey) = filterExtent f
                                     in ((floor (-ex), floor (-ey)), (ceiling (fromIntegral w+ex), ceiling (fromIntegral h+ey)))

film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA :: (Filter f, Show f) => f -> Film
film1080      = film (1920, 1080)
film720       = film (1280,  720)
film480       = film ( 720,  480)
film2k        = film (2048, 1080)
film4k        = film (4096, 2160)
film8k        = film (8192, 4608)
filmQVGA      = film ( 320,  240)
filmVGA       = film ( 640,  480)
