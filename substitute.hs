substitute :: (Eq a) => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (x:xs) s
    | x == w    = s ++ (substitute w xs s)
    | otherwise = [x] ++ (substitute w xs s)

--sub l w r = let (ys,zs) = splitAt (elemIndex) l in ys ++ r ++ zs

-- "3*cos(x) + 4 - x" 'x' "5.37"

-- substitute 'x' "3*cos(x) + 4 - x" "5.37"
