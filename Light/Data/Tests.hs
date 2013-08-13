module Light.Data.Tests (main) where

import qualified Test.QuickCheck as QC

import Light.Data.Vector hiding (fromList)
import Light.Data.Point  hiding (fromList)
import Light.Data.Matrix
import qualified Light.Data.Vector as V
import qualified Light.Data.Point  as P

instance QC.Arbitrary Vector where
  arbitrary = QC.vector 3 >>= return . V.fromList

instance QC.Arbitrary Point where
  arbitrary = QC.vector 3 >>= return . P.fromList

instance QC.Arbitrary Matrix where
  arbitrary = QC.vector 16 >>= return . matrix

prop_VectorAdditiveIdentity :: Vector -> Bool
prop_VectorAdditiveIdentity v = v == (v ^+^ zeroV)
                             && v == (v ^-^ zeroV)

prop_CrossProductIsOrthogonal :: Vector -> Vector -> Bool
prop_CrossProductIsOrthogonal u v = u == zeroV
                                 || v == zeroV
                                 || ( abs ((cross3 (normalized u) (normalized v)) <.> u) < 0.0001
                                   && abs ((cross3 (normalized u) (normalized v)) <.> v) < 0.0001 )

prop_MatrixMultiplicitiveIdentity :: Matrix -> Bool
prop_MatrixMultiplicitiveIdentity m = m |*| identity == m
                                   && identity |*| m == m

prop_MatrixMultiplicitiveZero :: Matrix -> Bool
prop_MatrixMultiplicitiveZero m = m |*| zero == zero
                               && zero |*| m == zero

prop_MatrixAdditiveIdentity :: Matrix -> Bool
prop_MatrixAdditiveIdentity m = m |+| zero == m
                             && zero |+| m == m

prop_MatrixTranspose :: Matrix -> Bool
prop_MatrixTranspose m = transpose (transpose m) == m

prop_VectorTimesZero :: Vector -> Bool
prop_VectorTimesZero v = v ^*| zero == zeroV
                      && zero |*^ v == zeroV

prop_PointTimesZero :: Point -> Bool
prop_PointTimesZero p = p .*| zero == origin
                     && zero |*. p == origin

prop_VectorTimesIdentity :: Vector -> Bool
prop_VectorTimesIdentity v = v ^*| identity == v
                          && identity |*^ v == v

prop_PointTimesIdentity :: Point -> Bool
prop_PointTimesIdentity p = p .*| identity == p
                         && identity |*. p == p

main = do
  QC.quickCheck prop_VectorAdditiveIdentity
  QC.quickCheck prop_CrossProductIsOrthogonal
  QC.quickCheck prop_MatrixMultiplicitiveIdentity 
  QC.quickCheck prop_MatrixMultiplicitiveZero 
  QC.quickCheck prop_MatrixAdditiveIdentity 
  QC.quickCheck prop_MatrixTranspose
  QC.quickCheck prop_VectorTimesZero
  QC.quickCheck prop_PointTimesZero
  QC.quickCheck prop_VectorTimesIdentity
  QC.quickCheck prop_PointTimesIdentity
  QC.quickCheck prop_MatrixTranspose
