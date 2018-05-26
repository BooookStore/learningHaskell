-- Functor は あるデータ型が持っている値を別の値へ変える
-- f は一つの型引数を取る型コンストラクタ
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- リストに対するFunctor
-- [] はリストの型コンストラクタ
instance Functor' [] where
    fmap' = map

-- Maybeに対するFunctor
instance Functor' Maybe where
    fmap' f (Just x) = Just (f x)
    fmap' f Nothing = Nothing

data Box a = EmptyBox | Box a deriving (Show)

instance Functor' Box where
    fmap' f (Box a) = Box (f a)
    fmap' f EmptyBox = EmptyBox

instance Functor' (Either a) where
    fmap' f (Right x) = Right (f x)
    fmap' f (Left x) = Left x