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