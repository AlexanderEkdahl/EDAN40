--maxi :: Ord a => a -> a -> a ?
maxi x y = if x > y then x else y

sumsq :: Num t => t -> t
sumsq 0 = 0
sumsq n = n^2 + sumsq(n - 1)

--sumsq' :: (Num a, Enum a) => a -> a ?
--sumsq' n = sum ( map sq [1..n] ) where sq x = x^2
sumsq' n
  | n <= 0     = 0
  | otherwise  = sum $ map (^2) [1..n]

sumsq'' n = sum $ map (^2) [1..n]

type Month = Integer
daysInMonth :: Month -> Integer -> Integer
daysInMonth m y
  | m <= 0 || m > 12   = 0
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12  = 31
  | m == 4 || m == 6 || m == 9 || m == 11   = 30
  | mod y 100 == 0  = 28
  | mod y 4 == 0  = 29
  | otherwise     = 28

data Date = Date Integer Month Integer

validDate :: Date -> Bool
validDate (Date d m y) =
    if (d > 0) && (d <= daysInMonth m y)
        then True
        else False

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute w r l =
    if head l == w
        then r : substitute w r ( tail l )
        else head l : substitute w r ( tail l )

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates l =
    if elem (head l) (tail l)
        then True
        else duplicates (tail l)

duplicates' :: Eq a => [a] -> Bool
duplicates' [] = False
duplicates' [x] = False
duplicates' (x:xs)
  | elem x xs = True
  | otherwise = duplicates' xs

isPermutation :: (Ord a) => [a] -> [a] -> Bool
isPermutation xs ys
    | sort xs == sort ys = True
    | otherwise = False
    where sort [] = []
          sort (x:xs) = ( sort [a | a <- xs, a <= x] ) ++ [x] ++ ( sort [a | a <- xs, a > x] )

initials :: [Char] -> [Char] -> [Char]
initials (f:_) (l:_) = f : ['.'] ++ [l] ++ ['.']
