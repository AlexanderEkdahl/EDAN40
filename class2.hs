import Data.Maybe

data Proposition = Var Name
                 | Proposition :&: Proposition
                 | Proposition :|: Proposition
                 | Not Proposition
  deriving ( Eq, Show )

type Name = String

-- uniques?
vars :: Proposition -> [Name]
vars (Var x)   = [x]
vars (a :&: b) = vars a ++ vars b
vars (a :|: b) = vars a ++ vars b
vars (Not a)   = vars a

-- truthValue (Var "q" :&: Var "p") [("q", True), ("p", True)]
truthValue :: Proposition -> [(Name, Bool)] -> Bool
truthValue (Var x) v   = fromJust (lookup x v)
truthValue (a :&: b) v = truthValue a v && truthValue b v
truthValue (a :|: b) v = truthValue a v || truthValue b v
truthValue (Not a) v   = not (truthValue a v)

-- tautology (Var "q" :|: Not (Var "q"))
tautology :: Proposition -> Bool
tautology p = and (map (truthValue p) (allVals (vars p)))

allVals :: [Name] -> [[(Name,Bool)]]
allVals []     = [[]]
allVals (x:xs) = [ (x,b):val | val <- allVals xs, b <- [False,True] ]

-- nice print function :)

data File = File Name | Dir Name [File] deriving ( Eq, Show )

search :: File -> Name -> Bool
search (Dir _ ns) x = any id (map (search x) ns)
search (File n) x = True

-- Dir "~" [File "a", Dir "b" [File "x", File "y"], File "c"]

-- search "x", (Dir "~" [File "a", Dir "b" [File "x", File "y"], File "c"])
