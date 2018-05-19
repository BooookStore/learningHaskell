data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- 値をひとつだけ持つ木を作成
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- 木に値を挿入
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
        | x == a = Node a left right
        | x < a =  Node a (treeInsert x left) right
        | x > a =  Node a left (treeInsert x right)

-- 値が木に含まれているかを判定する
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
        | x == a = True
        | x < a = treeElem x left
        | x > a = treeElem x right

-- Functor は あるデータ型が持っている値を別の値へ変える
-- f は一つの型引数を取る型コンストラクタ
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' Tree where
    fmap' f EmptyTree = EmptyTree
    fmap' f (Node x left right) = Node (f x) (fmap' f left) (fmap' f right)