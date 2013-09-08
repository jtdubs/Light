{-# LANGUAGE TemplateHaskell #-}

module Light.Film
  ( Film, film, filmWidth, filmHeight
  , film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA
  )
where

import Control.Lens

data Film = Film { _filmWidth :: Int, _filmHeight :: Int } deriving (Eq, Show, Read)

film :: Int -> Int -> Film
film = Film

makeLenses ''Film

film1080, film720, film480, film2k, film4k, film8k, filmQVGA, filmVGA :: Film
film1080      = film 1920 1080
film720       = film 1280  720
film480       = film  720  480
film2k        = film 2048 1080
film4k        = film 4096 2160
film8k        = film 8192 4608
filmQVGA      = film  320  240
filmVGA       = film  640  480
