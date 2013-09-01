module Main
where

import Light.Renderer
import Light.Scene
import Light.Camera
import Light.Shape
import Light.Geometry
import Light.Primitive

main :: IO ()
main = render $ scene (cameraBox $ perspectiveCamera film720 (pi/3))
                      [ primitive (sphere 1 `translate` Vector   0    0   5) Material
                      , primitive (sphere 1 `translate` Vector   5    0   8) Material
                      , primitive (sphere 1 `translate` Vector (-5)   4  10) Material
                      , primitive (sphere 1 `translate` Vector   8  (-3) 12) Material
                      ]
