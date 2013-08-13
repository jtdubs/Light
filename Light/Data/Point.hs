{-# LANGUAGE TypeFamilies #-}

module Light.Data.Point
    -- ADT
	( Point

	-- Construction
	, point, toList, fromList

	-- Default Instances
    , origin

	-- Data.AffineSpace
	, AffineSpace(..), (.-^), distanceSq, distance
	)
where

import Data.AffineSpace (AffineSpace(..), (.-^), distanceSq, distance)
import Data.List        (intersperse)

import qualified Light.Data.Vector as V

data Point = Point !Float !Float !Float

point :: Float -> Float -> Float -> Point
point = Point

fromList :: [Float] -> Point
fromList [x, y, z]    = Point  x     y     z
fromList [0, 0, 0, 0] = Point  0     0     0
fromList [x, y, z, w] = Point (x/w) (y/w) (z/w)

toList :: Point -> [Float]
toList (Point x y z) = [x, y, z, 1]

origin :: Point
origin = point 0 0 0

instance Eq Point where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Point where
  show v = "#P(" ++ (concat . intersperse ", " . map show . toList) v ++ ")"

instance AffineSpace Point where
  type Diff Point = V.Vector
  p .-. q = V.fromList $ zipWith (-) (toList p) (  toList q)
  p .+^ v =   fromList $ zipWith (+) (toList p) (V.toList v)
