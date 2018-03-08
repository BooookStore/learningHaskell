pointfree :: (Num a) => [a] -> a
pointfree = foldl (+) 0

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]