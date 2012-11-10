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
-- match '*' "I need *" "I need you to kill yourself"
-- match "*" ["I","need","*"] ["I","need","you","to","kill","yourself"]
-- mmap reflect (match "*" ["I","need","*"] ["I","need","you","to","kill","yourself"])
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _  = Nothing
match _ _ []  = Nothing
match wc p s
  | head p == wc = orElse (singleWildcardMatch p s) (longerWildcardMatch p s)
  | head p == head s = match wc (tail p) (tail s)
  | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch _ [] = Nothing
singleWildcardMatch [] _ = Nothing
singleWildcardMatch (wc:ps) (x:xs)
  | ps == xs = (Just [x])
  | otherwise = Nothing

longerWildcardMatch _ [] = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:ps) xs
  | reverse (take (length ps) (reverse xs)) == ps = Just (take (length xs - length ps) xs)
  | otherwise = Nothing

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
-- transformationApply '*' (unwords . reflect . words)  "I need you to kill yourself" ("I need *", "Why do you need * ?")
-- transformationApply '*' id "please kill yourself" ("please *", "*")
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f l t = mmap (substitute wc (snd t)) (mmap f (match wc (fst t) l))

-- Applying a list of patterns until one succeeds
-- transformationsApply '*' (unwords . reflect . words) [("I need *", "Why do you need * ?")] "I need you to kill yourself"
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f t l = orElse (transformationApply wc f l (head t)) (transformationsApply wc f (tail t) l)
