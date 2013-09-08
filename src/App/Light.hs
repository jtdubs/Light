module Main
where

import Light.Renderer
import Light.Scene
import Light.Cameras
import Light.Shapes
import Light.Geometry
import Light.Primitive

theCamera :: CameraBox
theCamera = cameraBox $ perspectiveCamera film720 (pi/3)

union :: UnionShape
union = unionShape [ shapeBox $ unitSphere      `translate` Vector 0 0 0
                   , shapeBox $ box 1.1 0.6 0.6 `translate` Vector 0 0 (-0.6) ]

intersection :: IntersectionShape
intersection = intersectionShape [ shapeBox $ unitSphere      `translate` Vector 0 0 0
                                 , shapeBox $ box 1.1 0.6 0.6 `translate` Vector 0 0 (-0.6) ]

disjunction :: DisjunctionShape
disjunction = disjunctionShape [ shapeBox $ unitSphere      `translate` Vector 0 0 0 ]
                               [ shapeBox $ box 1.1 0.6 0.6 `translate` Vector 0 0 (-0.6) ]

theScene :: Scene
theScene = scene theCamera
                 [ primitive (unitTriangle   `rotate3` (-pi/4,     0, 0) `translate` Vector (-4.0)   3  10) Material
                 , primitive (unitPlane      `rotate3` (-pi/4,     0, 0) `translate` Vector ( 0.0)   3  10) Material
                 , primitive (unitDisc       `rotate3` (-pi/4,     0, 0) `translate` Vector ( 4.0)   3  10) Material

                 , primitive (disjunction    `rotate3` (    0, -pi/2, 0) `translate` Vector (-4.0)   0  10) Material
                 , primitive (intersection   `rotate3` (    0, -pi/2, 0) `translate` Vector ( 0.0)   0  10) Material
                 , primitive (union          `rotate3` (    0, -pi/2, 0) `translate` Vector ( 4.0)   0  10) Material

                 , primitive (unitBox        `rotate3` (    0,     0, 0) `translate` Vector (-6.0) (-3) 10) Material
                 , primitive (unitSphere     `rotate3` (    0,     0, 0) `translate` Vector (-3.0) (-3) 10) Material
                 , primitive (cylinder   1 2 `rotate3` ( pi/2,     0, 0) `translate` Vector ( 0.0) (-2) 10) Material
                 , primitive (paraboloid 1 2 `rotate3` (-pi/2,     0, 0) `translate` Vector ( 3.0) (-4) 10) Material
                 , primitive (cone       1 2 `rotate3` ( pi/2,     0, 0) `translate` Vector ( 6.0) (-2) 10) Material
                 ]

main :: IO ()
main = render theScene
