type CompressedData = ([String], [Int])

compress :: String -> CompressedData
compress s = foldl f ([], []) (words s)
  where
    f (u, i) n
      | elem n u  = (u, i ++ [g n u 0])
      | otherwise = (u ++ [n], i ++ [length u])
    g x (y:ys) i
      | x == y    = i
      | otherwise = g x ys (i + 1)

decompress :: CompressedData -> String
decompress (u, i) = unwords $ map (u !!) i
