module Light.Camera.Film
  ( Film(..)
  )
where

data Film = Film { filmWidth :: Int, filmHeight :: Int } deriving (Eq)

instance Show Film where
  show (Film w h) = concat ["#F(", show w, ", ", show h, ")"]
