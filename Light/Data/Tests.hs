module Light.Data.Tests (main) where

import qualified Test.QuickCheck as QC

import Light.Data.Vector
import Light.Data.Point
import Light.Data.Matrix

instance QC.Arbitrary Vector where
  arbitrary = QC.vector 3 >>= return . vector

instance QC.Arbitrary Point where
  arbitrary = QC.vector 3 >>= return . point

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
prop_MatrixMultiplicitiveIdentity m = m |*| identityMatrix == m
                                   && identityMatrix |*| m == m

main = do
  QC.quickCheck prop_VectorAdditiveIdentity
  QC.quickCheck prop_CrossProductIsOrthogonal
  QC.quickCheck prop_MatrixMultiplicitiveIdentity 
