type CompressedData = ([String], [Int])

-- > compress "This specification is the specification for a specification"
-- (["This","specification","is","the","for","a"],[0,1,2,3,1,4,5,1])
compress :: String -> CompressedData
compress s = foldl f ([], []) (words s)
  where
    f (u, i) n
      | elem n u  = (u, i ++ [g n u 0])
      | otherwise = (u ++ [n], i ++ [length u])
    g x (y:ys) i
      | x == y    = i
      | otherwise = g x ys (i + 1)

-- > decompress (["This","specification","is","the","for","a"],[0,1,2,3,1,4,5,1])
-- "This specification is the specification for a specification"
decompress :: CompressedData -> String
decompress (u, i) = unwords $ map (u !!) i
