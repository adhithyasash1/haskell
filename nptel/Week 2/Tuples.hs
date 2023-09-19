-- Tuples

sumpairs :: (Int, Int) -> Int
sumpairs (x,y) = x+y

sumpairlist :: [(Int, Int)] -> Int
sumpairlist [] = 0
sumpairlist (x,y):zs = x + y + sumpairlist zs


-- Looking up a student's mark out of marks list

lookup :: String -> [(String, Int)] -> Int
lookup p [] = -1
lookup p ((name,marks):ms)
 | (p == name) = marks
 | otherwise = lookup p ms


-- Type Aliases

type Point2D = (Float, Float)

distance2D :: Point2D -> Point2D -> Float
distance2D (x1,y1) (x2,y2) = sqrt(sqr (x1-x1) + sqr (y2-y1))
where
sqr x :: Float -> Float
sqr x = x*x

myDistance2D :: Point2D -> Point2D -> Float
myDistance2D (x1,y1) (x2,y2) = sqrt(xdiff*xdiff + ydiff*ydiff)
where
xdiff :: Float
xdiff = x2-x1
ydiff :: Float
ydiff = y2-y1


type Point3D = (Float, Float, Float)

distance3D :: Point3D -> Point3D -> Float
distance3D (x1,y1,z1) (x2,y2,z2) = sqrt(sqr (x1-x1) + sqr (y2-y1) + sqr (z2-z1))
where
sqr x :: Float -> Float
sqr x = x*x
