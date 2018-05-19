data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float |
             Rectangle Point Point
             deriving (Show)

move :: Point -> Float -> Float -> Point
move (Point x y) x1 y1 = Point (x+x1) (y+y1)

moveShape :: Shape -> Float -> Float -> Shape
moveShape (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
moveShape (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

sizeChangeShape :: Shape -> Float -> Shape
sizeChangeShape (Circle (Point x y) r) a = Circle (Point x y) (r+a)
sizeChangeShape (Rectangle (Point x1 y1) (Point x2 y2)) a = Rectangle (Point x1 y1) (Point (x2+a) (y2+a))

-- 型コンストラクタ
nonumber :: Maybe Int
nonumber = Nothing

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Int
                     , phoneNumber :: String
                     , flavor:: String } deriving (Show)

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y }) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data Day = Monday | TuesDay | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- 型シノニム
-- 型に別の名前を付けて可読性を向上させる。
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- 型シノニムを利用して関数を定義
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phoneNumber pBook = (name, phoneNumber) `elem` pBook