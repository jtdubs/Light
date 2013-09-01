module Light.Geometry.PointTest (tests) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Light.Geometry.Point
import Light.Geometry.Vector

import Light.Geometry.VectorTest ()

instance QC.Arbitrary Point where
  arbitrary = QC.vector 3 >>= \ [a, b, c] -> return $ Point a b c

prop_PointAdditiveIdentity :: Point -> Bool
prop_PointAdditiveIdentity p = p == (p .+^ zeroVector)
                            && p == (p .-^ zeroVector)

prop_PointMinusSelfIsZero :: Point -> Bool
prop_PointMinusSelfIsZero p = p .-. p == zeroVector

prop_PointDistance :: Point -> Vector -> Bool
prop_PointDistance p v = abs (distance p (p .+^ v) - magnitudeV v) < 0.00001

tests :: [Test]
tests = [ testProperty "PointAdditiveIdentity" prop_PointAdditiveIdentity
        , testProperty "PointMinusSelfIsZero" prop_PointMinusSelfIsZero
        , testProperty "PointDistance" prop_PointDistance
        ]
