module Geometry where

d2r = (* (pi / 180))
r2d = (* (180 / pi))

data Line a = Line { _a :: a, _b :: a , _c :: a } deriving (Show)
data Point a = Point { _x :: a, _y :: a, _z :: a } deriving (Show)

line (a, b, c) = Line a b c
_point (x, y, z) = Point x y z

det (a, b) (x, y) = a * y - b * x
cp (a, b, c) (x, y, z) = (det (b, c) (y, z),
                          det (c, a) (z, x),
                          det (a, b) (x, y))

link (Point a b c) (Point x y z) = line (cp (a, b, c) (x, y, z))
cross (Line a b c) (Line x y z) = _point (cp (a, b, c) (x, y, z))

point x y = _point (x, y, 1)

coord (Point x y z) = (x / z, y / z)

-- | y - py = k (x - px)
ypkx p k = l (coord p) k where l (a, b) k = line (k, -1, b - k * a)



ed (a, b) (x, y) = sqrt (q (a - x) + q (b - y)) where q x = x * x
dis p q = ed (coord p) (coord q)

angle b a c = acos ((y * y + z * z - x * x) / (2 * y * z))
  where (x, y, z) = (dis b c, dis c a, dis a b)
