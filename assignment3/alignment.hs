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

--sim (x:xs) (y:ys) = maximum [sim(xs ys) + score(x,y), sim(xs,(y:ys)) + score(x,'-'), sim((x:xs),ys) + score('-',y)]
sim [] _ = 0
sim _ [] = 0
sim (x:xs) (y:ys) = maximum [ (sim xs ys) + (score x y), (sim xs (y:ys)) + (score x '-'), (sim (x:xs) ys) + (score '-' y) ]

-- Attaches h1 and h2 to the front of each of the strings the the pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 list = [(h1:xs, h2:ys) | (xs,ys) <- list]

--similarityScore :: String -> String -> Int
--similarityScore string1 string2
