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
                      [ primitive (unitTriangle   `rotate3` (-pi/4, 0, 0) `translate` Vector (-5)   3  10) Material
                      , primitive (unitPlane      `rotate3` (-pi/4, 0, 0) `translate` Vector   0    3  10) Material
                      , primitive (unitDisc       `rotate3` (-pi/4, 0, 0) `translate` Vector   5    3  10) Material
                      , primitive (unitSphere     `rotate3` (    0, 0, 0) `translate` Vector   0    0  10) Material
                      , primitive (cylinder   1 2 `rotate3` ( pi/2, 0, 0) `translate` Vector (-5) (-2) 10) Material
                      , primitive (paraboloid 1 2 `rotate3` (-pi/2, 0, 0) `translate` Vector   0  (-4) 10) Material
                      , primitive (cone       1 2 `rotate3` ( pi/2, 0, 0) `translate` Vector   5  (-2) 10) Material
                      ]
