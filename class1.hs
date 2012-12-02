max' x y
  | x > y     = x
  | otherwise = y

sumsq n
  | n > 0     = n^2 + sumsq (n - 1)
  | otherwise = 0

sumsq' n
  | n > 0     = foldl1 (+) $ map (^2) [1..n]
  | otherwise = 0
