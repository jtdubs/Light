{-# LANGUAGE TypeFamilies #-}

module Light.Data.Vector
	( Vector, vector, toList
    , unitX, unitY, unitZ

	-- Data.AdditiveGroup
    , AdditiveGroup(..), (^-^), sumV

	-- Data.Cross
    , HasCross3(..)

	-- Data.VectorSpace
    , VectorSpace(..), (^*), magnitudeSq, magnitude, normalized
    , InnerSpace(..)
	)
where

import Data.AdditiveGroup (AdditiveGroup(..), (^-^), sumV)
import Data.Basis         (HasBasis(..))
import Data.Cross         (HasCross3(..))
import Data.VectorSpace   (VectorSpace(..), InnerSpace(..), (*^), (^*), (<.>), magnitudeSq, magnitude, normalized)

data Vector = Vector { vx :: !Float, vy :: !Float, vz :: !Float }

vector :: [Float] -> Vector
vector [x, y, z]    = Vector x y z
vector [x, y, z, 0] = Vector x y z

toList :: Vector -> [Float]
toList (Vector x y z) = [x, y, z, 0]

unitX, unitY, unitZ :: Vector
unitX = vector [1, 0, 0]
unitY = vector [0, 1, 0]
unitZ = vector [0, 0, 1]

instance Eq Vector where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Vector where
  show (Vector a b c) = concat ["#V(", show a, ", ", show b, ", ", show c, ")"]

instance AdditiveGroup Vector where
  zeroV = vector [0, 0, 0]
  u ^+^ v = vector $ zipWith (+) (toList u) (toList v)
  negateV = vector . map negate . toList

instance VectorSpace Vector where
  type Scalar Vector =  Float
  s *^ v = vector $ map (*s) $ toList v

instance InnerSpace Vector where
  u <.> v = sum $ zipWith (*) (toList u) (toList v)

instance HasCross3 Vector where
  cross3 (Vector a b c) (Vector d e f) = vector [b*f-c*e, c*d-a*f, a*e-b*d]
