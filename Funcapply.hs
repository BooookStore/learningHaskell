fapply1 :: [Int] -> Int
fapply1 xs = sum $ filter (>10) (map (*2) xs)
