{-# LANGUAGE TemplateHaskell #-}

module Light.Entity.Sphere
    -- ADT
    ( Sphere, sphere, center, radius

    -- Default Instances
    , unitSphere
    )
where

import Control.Lens     ((%~))
import Control.Lens.TH  (makeLenses)
import Light.Math.Point (Point, originPoint)
import Light.Math.Basis (Basis, defaultBasis, HasBasis(..), Orientable(..))

data Sphere = Sphere { _sphereBasis :: Basis, _center :: Point, _radius :: Float } deriving (Eq)

sphere = Sphere defaultBasis

makeLenses ''Sphere

instance Show Sphere where
  show (Sphere b c r) = concat ["#S(", show b, ", ", show c, ", ", show r, ")"]

instance HasBasis Sphere where
  basis = sphereBasis

instance Orientable Sphere where
  outOf c = basis %~ outOf c 
  into  c = basis %~ into  c

unitSphere = sphere originPoint 1
