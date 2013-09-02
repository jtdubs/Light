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
                      [ primitive (sphere     1                           `translate` Vector   0    0   5) Material
                      , primitive (cone       1 3 `rotate3` ( pi/2, 0, 0) `translate` Vector   5    3   8) Material
                      , primitive (cylinder   1 3 `rotate3` ( pi/2, 0, 0) `translate` Vector (-5)   4  10) Material
                      , primitive (paraboloid 1 3 `rotate3` (-pi/2, 0, 0) `translate` Vector   8  (-4) 12) Material
                      ]
