{-# LANGUAGE ExistentialQuantification #-}

module Light.Filter
  ( Filter(..)
  , FilterBox, filterBox
  )
where

class Filter a where
  filterExtent :: a -> (Double, Double)
  filterWeight :: a -> (Double, Double) -> Double

data FilterBox = forall f. (Filter f, Show f) => FilterBox f

filterBox :: (Filter f, Show f) => f -> FilterBox
filterBox = FilterBox

instance Show FilterBox where
  show (FilterBox f) = show f

instance Filter FilterBox where
  filterExtent (FilterBox f) = filterExtent f
  filterWeight (FilterBox f) = filterWeight f
