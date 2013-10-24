--mcsLength' :: Eq a => [a] -> [a] -> Int
--mcsLength' _ [] = 0
--mcsLength' [] _ = 0
--mcsLength' (x:xs) (y:ys)
--  | x == y    = 1 + mcsLength' xs ys
--  | otherwise = max (mcsLength' xs (y:ys)) (mcsLength' (x:xs) ys)

--mcsLength :: Eq a => [a] -> [a] -> Int
--mcsLength xs ys = mcsLen (length xs) (length ys)
--  where
--     mcsLen i j = mcsTable!!i!!j
--     mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

--     mcsEntry :: Int -> Int -> Int
--     mcsEntry _ 0 = 0
--     mcsEntry 0 _ = 0
--     mcsEntry i j
--       | x == y    = 1 + mcsLen (i-1) (j-1)
--       | otherwise = max (mcsLen i (j-1)) (mcsLen (i-1) j)
--       where
--          x = xs!!(i-1)
--          y = ys!!(j-1)

scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
  | x == y    = scoreMatch
  | otherwise = scoreMismatch

similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (x:xs) (y:ys) = maximum [ (similarityScore xs ys) + (score x y), 
										  (similarityScore xs (y:ys)) + (score x '-'),
										  (similarityScore (x:xs) ys) + (score '-' y) ]

-- Attaches h1 and h2 to the front of each of the strings the the pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 list = [(h1:xs, h2:ys) | (xs,ys) <- list]

--maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
--maximaBy f (x:xs) = 

--maximaBy' (y:ys) (x:xs)
--	| v > r     = maximaBy' v [x] xs
--	| v == r    = maximaBy' v [y:ys:x] xs
--	| otherwise = maximaBy' r [y:ys] xs
--	where v = length x
--		  r = length y

maximaBy _ [] = []
maximaBy [] _ = []
maximaBy (y:ys) (x:xs)  
    | v > r         = maximaBy [x] xs
    | v == r        = maximaBy ([y] ++ ys ++ [x]) xs
    | otherwise     = maximaBy ([y] ++ ys) xs
    where v = length x
          r = length y

-- | v > r     = maximaBy' v [x] xs
-- | v == r    = maximaBy' v [y:ys:x] xs
-- | otherwise = maximaBy' r [y:ys] xs
-- where v = f x
--      r = f y

--maximum --maximum [ f x | x <- xs]  

--similarityScore :: String -> String -> Int
--similarityScore string1 string2
