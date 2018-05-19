module Shape
( Point
, Shape
, area
, nudge
, basicCircle
, basicRect
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float |
             Rectangle Point Point 
      deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (x + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
                                 = Rectangle (Point (x1 + a) (y1 + a)) (Point (x2 + a) (y2 + a))

basicCircle :: Float -> Shape
basicCircle r = Circle (Point 0 0) r

basicRect :: Float -> Float -> Shape
basicRect width height = Rectangle (Point 0 0) (Point width height)
