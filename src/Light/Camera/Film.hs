module Light.Camera.Film
  ( Film(..)
  , film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA
  )
where

data Film = Film { filmWidth :: Int, filmHeight :: Int } deriving (Eq)

instance Show Film where
  show (Film w h) = concat ["#F(", show w, ", ", show h, ")"]

film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA :: Film
film1080      = Film 1920 1080
film720       = Film 1280  720
film480       = Film  720  480
film2k        = Film 2048 1080
film4k        = Film 4096 2160
film8k        = Film 8192 4608
filmQVGA      = Film  320  240
filmVGA       = Film  640  480
