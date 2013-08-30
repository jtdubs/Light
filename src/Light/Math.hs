module Light.Math
  ( quadratic 
  )
where

import Data.List

quadratic :: Float -> Float -> Float -> Maybe [Float]
quadratic a b c = if d < 0
                  then Nothing
                  else Just $ sort [ q/a, c/q ]
  where d = b*b - 4*a*c
        q = if b < 0
            then -(b - sqrt d)/2
            else -(b + sqrt d)/2
