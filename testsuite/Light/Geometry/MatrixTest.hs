module Light.Geometry.MatrixTest (tests) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Light.Geometry.Matrix
import Light.Geometry.Vector
import Light.Geometry.Point

import Light.Geometry.VectorTest ()
import Light.Geometry.PointTest ()

instance QC.Arbitrary Matrix where
  arbitrary = QC.vector 16 >>= \ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] -> (return $ Matrix a b c d e f g h i j k l m n o p)

prop_MatrixMultiplicitiveIdentity :: Matrix -> Bool
prop_MatrixMultiplicitiveIdentity m = m |*| identityMatrix == m
                                   && identityMatrix |*| m == m

prop_MatrixMultiplicitiveZero :: Matrix -> Bool
prop_MatrixMultiplicitiveZero m = m |*| zeroMatrix == zeroMatrix
                               && zeroMatrix |*| m == zeroMatrix

prop_MatrixAdditiveIdentity :: Matrix -> Bool
prop_MatrixAdditiveIdentity m = m |+| zeroMatrix == m
                             && zeroMatrix |+| m == m

prop_MatrixTranspose :: Matrix -> Bool
prop_MatrixTranspose m = transpose (transpose m) == m

prop_VectorTimesZero :: Vector -> Bool
prop_VectorTimesZero v = v ^*| zeroMatrix == zeroVector
                      && zeroMatrix |*^ v == zeroVector

prop_PointTimesZero :: Point -> Bool
prop_PointTimesZero p = p .*| zeroMatrix == originPoint
                     && zeroMatrix |*. p == originPoint

prop_VectorTimesIdentity :: Vector -> Bool
prop_VectorTimesIdentity v = v ^*| identityMatrix == v
                          && identityMatrix |*^ v == v

prop_PointTimesIdentity :: Point -> Bool
prop_PointTimesIdentity p = p .*| identityMatrix == p
                         && identityMatrix |*. p == p

tests :: [Test]
tests = [ testProperty "MatrixMultiplicitiveIdentity" prop_MatrixMultiplicitiveIdentity
        , testProperty "MatrixMultiplicitiveZero" prop_MatrixMultiplicitiveZero
        , testProperty "MatrixAdditiveIdentity" prop_MatrixAdditiveIdentity
        , testProperty "MatrixTranspose" prop_MatrixTranspose
        , testProperty "VectorTimesZero" prop_VectorTimesZero
        , testProperty "PointTimesZero" prop_PointTimesZero
        , testProperty "VectorTimesIdentity" prop_VectorTimesIdentity
        , testProperty "PointTimesIdentity" prop_PointTimesIdentity
        ]
