{-# LANGUAGE TypeFamilies #-}

module Light.Data.Vector
    -- ADT
	( Vector

	-- Construction
	, vector, toList, fromList

	-- Default Instances
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
import Data.List          (intersperse)

data Vector = Vector !Float !Float !Float

vector :: Float -> Float -> Float -> Vector
vector = Vector

toList :: Vector -> [Float]
toList (Vector x y z) = [x, y, z, 0]

fromList :: [Float] -> Vector
fromList [x, y, z]    = vector x y z
fromList [x, y, z, 0] = vector x y z

unitX, unitY, unitZ :: Vector
unitX = vector 1 0 0
unitY = vector 0 1 0
unitZ = vector 0 0 1

instance Eq Vector where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Vector where
  show v = "#V(" ++ (concat . intersperse ", " . map show . toList) v ++ ")"

instance AdditiveGroup Vector where
  zeroV = vector 0 0 0
  u ^+^ v = fromList $ zipWith (+) (toList u) (toList v)
  negateV = fromList . map negate . toList

instance VectorSpace Vector where
  type Scalar Vector =  Float
  s *^ v = fromList $ map (*s) $ toList v

instance InnerSpace Vector where
  u <.> v = sum $ zipWith (*) (toList u) (toList v)

instance HasCross3 Vector where
  cross3 u v = let [ux, uy, uz, _] = toList u
                   [vx, vy, vz, _] = toList v
                 in vector (uy*vz - uz*vy) (uz*vx - ux*vz) (ux*vy - uy*vx)
