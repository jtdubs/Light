module Main
where

import Light.Renderer
import Light.Scene
import Light.Camera
import Light.Shape
import Light.Geometry
import Light.Primitive

main :: IO ()
main = render $ scene (cameraBox $ perspectiveCamera filmQVGA (pi/3))
                      [ primitive (sphere 1 `translate` vector 0 0 5) Material
                      ]
