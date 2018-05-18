data TraficLight = Red | Yellow | Blue

instance Show TraficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Blue = "Blue light"

class Next a where
    ok :: a -> Bool

instance Next TraficLight where
    ok Red = False
    ok Yellow = False
    ok Blue = True

data Point a = Point a a

class Move a where
    move :: a -> Num -> a

instance (Num a) => Move (Point a) where
    move (Point x y) t = Point (x+t) (y+t)