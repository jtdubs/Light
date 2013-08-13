{-# LANGUAGE TypeFamilies #-}

module Light.Data.Point
	( Point, point, toList
    , origin

	-- Data.AffineSpace
	, AffineSpace(..), (.-^), distanceSq, distance
	)
where

import Data.AffineSpace (AffineSpace(..), (.-^), distanceSq, distance)
import qualified Light.Data.Vector as V

data Point = Point { px :: !Float, py :: !Float, pz :: !Float }

point :: [Float] -> Point
point [x, y, z]    = Point  x     y     z
point [x, y, z, w] = Point (x/w) (y/w) (z/w)

toList :: Point -> [Float]
toList (Point x y z) = [x, y, z, 1]

origin :: Point
origin = point [0, 0, 0]

instance Eq Point where
  u == v = all (< 0.00001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Point where
  show (Point a b c) = concat ["#P(", show a, ", ", show b, ", ", show c, ")"]

instance AffineSpace Point where
  type Diff Point = V.Vector
  p .-. q = V.vector $ zipWith (-) (toList p) (  toList q)
  p .+^ v = point    $ zipWith (+) (toList p) (V.toList v)
