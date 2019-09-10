module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where


sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)


sphereArea radius = 4 * pi * (radius ^ 2)


cubeVolume side = cuboidVolume side side side


cubeArea side = cuboidArea side side side


cuboidVolume a b c = rectangleArea a b * c


cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2


rectangleArea a b = a * b
