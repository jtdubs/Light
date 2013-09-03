module Main
where

import Light.Renderer
import Light.Scene
import Light.Camera
import Light.Shape
import Light.Geometry
import Light.Primitive

theCamera :: CameraBox
theCamera = cameraBox $ perspectiveCamera film720 (pi/3)

union :: UnionShape
union = unionShape [ shapeBox $ unitSphere `translate` Vector   0.5  0 0
                   , shapeBox $ unitSphere `translate` Vector (-0.5) 0 0
                   ]

intersection :: IntersectionShape
intersection = intersectionShape [ shapeBox $ unitSphere `translate` Vector   0.5  0 0
                                 , shapeBox $ unitSphere `translate` Vector (-0.5) 0 0
                                 ]

theScene :: Scene
theScene = scene theCamera
                 [ primitive (unitTriangle   `rotate3` (-pi/4, 0, 0) `translate` Vector (-5)     3  10) Material
                 , primitive (unitPlane      `rotate3` (-pi/4, 0, 0) `translate` Vector   0      3  10) Material
                 , primitive (unitDisc       `rotate3` (-pi/4, 0, 0) `translate` Vector   5      3  10) Material
                 , primitive (union          `rotate3` (    0, 0, 0) `translate` Vector (-7.5)   0  10) Material
                 , primitive (unitSphere     `rotate3` (    0, 0, 0) `translate` Vector (-2.5)   0  10) Material
                 , primitive (unitBox        `rotate3` (    0, 0, 0) `translate` Vector   2.5    0  10) Material
                 , primitive (intersection   `rotate3` (    0, 0, 0) `translate` Vector   7.5    0  10) Material
                 , primitive (cylinder   1 2 `rotate3` ( pi/2, 0, 0) `translate` Vector (-5)   (-2) 10) Material
                 , primitive (paraboloid 1 2 `rotate3` (-pi/2, 0, 0) `translate` Vector   0    (-4) 10) Material
                 , primitive (cone       1 2 `rotate3` ( pi/2, 0, 0) `translate` Vector   5    (-2) 10) Material
                 ]

main :: IO ()
main = render theScene
