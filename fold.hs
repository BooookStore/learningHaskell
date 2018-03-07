sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
