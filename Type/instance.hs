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
    move :: a -> Float -> a

instance (Float a) => Move (Float a) where
    move (Point x y) a = Point (x+a) (y+a)