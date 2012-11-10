module Utilities where

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)
-- Takes 2 tuples. One holding 2 functions and the other tuple holding 2 valid
-- arguments for the 2 functions in the previous tuple. Outputs the results of
-- the two functions running on the arguments in the second tuple.

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)
-- main = print (mmap square (Just 2))
-- If the second argument is unequal to Nothing the function f is run on value of the second argument
-- can be used on curried(?) functions example.. transformationApply wc f l t = mmap (substitute wc (snd t)) (match wc (fst t) l)
-- where substitute is lacking one argument(that is later applied with match)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
-- Returns the first argument if unequal to Nothing - otherwise it returns the second argument

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)
-- Recursively runs the result of the function untill the parameter matches the result of the function
-- Returns the matching x

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
-- Picks an element from xs depending on u being from 0(the first element) to 1 the last element
