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