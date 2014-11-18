maxi :: (Num a, Ord a) => a -> a -> a
maxi x y
  | x >= y     = x
  | otherwise = y

sumsq :: (Num a, Eq a) => a -> a
sumsq 0 = 0
sumsq n = n*n + sumsq(n - 1)

sumsq' :: (Num a, Enum a) => a -> a
sumsq' n = sum [x*x | x <- [1..n]]

hanoi 1 = 1
hanoi n = 2 * hanoi (n - 1) + 1

multiply :: (Num a) => [a] -> a
multiply = foldl1 (*)

substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute a b (x:xs)
  | x == a    = b : substitute a b xs
  | otherwise = x : substitute a b xs
--
-- "eigenvalue"
-- x == a
