-- Functor は あるデータ型が持っている値を別の値へ変える
-- f は一つの型引数を取る型コンストラクタ
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' (Either a) where
    fmap' f (Right x) = Right (f x)
    fmap' f (Left a) = Left a