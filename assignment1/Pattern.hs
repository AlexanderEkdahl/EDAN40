module Pattern where
import Utilities

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (x:xs) s
  | x == wc   = s ++ (substitute wc xs s)
  | otherwise = [x] ++ (substitute wc xs s)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _  = Nothing
match _ _ []  = Nothing
match wc p s
  | head p == wc = orElse (singleWildcardMatch p s) (longerWildcardMatch p s)
  | head p == head s = match wc (tail p) (tail s)
  | otherwise = Nothing

singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = f (match wc ps xs) x
  where f Nothing _ = Nothing
        f _ x = Just [x]

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f l t = mmap (substitute wc (snd t)) (mmap f (match wc (fst t) l))

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f t l = orElse (transformationApply wc f l (head t)) (transformationsApply wc f (tail t) l)
