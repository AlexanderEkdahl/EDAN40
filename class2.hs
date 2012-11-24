type Proposition = [(String,Bool)]

vars :: Proposition -> [String]
vars = map fst

--truthValue :: Proposition -> [(String,Bool)] -> Bool
truthValue p m = map (flip lookup m) (vars p)
